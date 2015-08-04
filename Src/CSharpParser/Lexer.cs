using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Microsoft.CodeAnalysis.CSharp;

namespace CSharpParser {

    public sealed class Lexer {
        [ThreadStatic]
        private static Lexer _instance;
        private static Lexer Instance {
            get { return _instance ?? (_instance = new Lexer()); }
        }
        public static Lexer Get(string filePath, TextReader reader, ParsingContext context, IEnumerable<string> ppSymbols) {
            return Instance.Init(filePath, reader, context, ppSymbols);
        }
        private Lexer() {
            _buf = new char[_bufLength];
        }
        //inputs
        private string _filePath;
        private TextReader _reader;
        private ParsingContext _context;
        private IEnumerable<string> _ppSymbols;
        private HashSet<string> _ppSymbolSet;
        private HashSet<string> PpSymbolSet {
            get {
                return _ppSymbolSet ?? (_ppSymbolSet = (_ppSymbols == null ? new HashSet<string>() : new HashSet<string>(_ppSymbols)));
            }
        }
        //
        private const int _bufLength = 1024;
        private readonly char[] _buf;
        private int _index, _count;
        private bool _isEOF;
        private int _totalIndex;
        private int _lastLine, _lastColumn, _line, _column;
        private const int _stringBuilderCapacity = 256;
        private StringBuilder _stringBuilder;
        private bool _atLineHead;//whitespace  is allowed 
        private bool _getNonTrivalToken;
        private int _ppRegionCount;
        private Stack<PpCondition> _ppConditionStack;
        private Stack<PpCondition> PpConditionStack {
            get {
                return _ppConditionStack ?? (_ppConditionStack = new Stack<PpCondition>());
            }
        }

        private Lexer Init(string filePath, TextReader reader, ParsingContext context, IEnumerable<string> ppSymbols) {
            if (filePath == null) throw new ArgumentNullException("filePath");
            if (reader == null) throw new ArgumentNullException("reader");
            if (context == null) throw new ArgumentNullException("context");
            _filePath = filePath;
            _reader = reader;
            _context = context;
            _ppSymbols = ppSymbols;
            _index = _count = 0;
            _isEOF = false;
            _totalIndex = 0;
            _lastLine = _lastColumn = _line = _column = 1;
            if (_stringBuilder == null) {
                _stringBuilder = new StringBuilder(_stringBuilderCapacity);
            }
            _atLineHead = true;
            _getNonTrivalToken = false;
            _ppRegionCount = 0;
            _ppExprToken = null;
            return this;
        }
        public void Clear() {
            _filePath = null;
            _reader = null;
            _context = null;
            _ppSymbols = null;
            _ppSymbolSet = null;
            if (_stringBuilder != null && _stringBuilder.Capacity > _stringBuilderCapacity * 8) {
                _stringBuilder = null;
            }
            if (_ppConditionStack != null) {
                _ppConditionStack.Clear();
            }
        }
        private StringBuilder GetStringBuilder() {
            return _stringBuilder.Clear();
        }
        private char GetChar(int offset = 0) {
            var pos = _index + offset;
            if (pos < _count) {
                return _buf[pos];
            }
            if (_isEOF) {
                return char.MaxValue;
            }
            var remainCount = _count - _index;
            if (remainCount > 0) {
                for (var i = 0; i < remainCount; ++i) {
                    _buf[i] = _buf[_index + i];
                }
            }
            var retCount = _reader.Read(_buf, remainCount, _bufLength - remainCount);
            if (retCount == 0) {
                _isEOF = true;
            }
            _index = 0;
            _count = remainCount + retCount;
            return GetChar(offset);
        }
        private char GetNextChar() {
            return GetChar(1);
        }
        private char GetNextNextChar() {
            return GetChar(2);
        }
        private void AdvanceChar(bool checkNewLine) {
            _lastLine = _line;
            _lastColumn = _column;
            if (_index < _count) {
                if (checkNewLine) {
                    var ch = _buf[_index++];
                    ++_totalIndex;
                    if (SyntaxFacts.IsNewLine(ch)) {
                        if (ch == '\r' && GetChar() == '\n') {
                            ++_index;
                            ++_totalIndex;
                        }
                        ++_line;
                        _column = 1;
                    }
                    else {
                        ++_column;
                    }
                }
                else {
                    ++_index;
                    ++_totalIndex;
                    ++_column;
                }
            }
        }
        private int _tokenStartIndex;
        private TextPosition _tokenStartPosition;
        private void MarkTokenStart() {
            _tokenStartIndex = _totalIndex;
            _tokenStartPosition = new TextPosition(_line, _column);
        }
        private TextSpan CreateFullTextSpan() {
            var startIndex = _tokenStartIndex;
            return new TextSpan(_filePath, startIndex, _totalIndex - startIndex, _tokenStartPosition,
                new TextPosition(_lastLine, _lastColumn));
        }
        private Token CreateToken(TokenKind tokenKind, string value = null) {
            //if (tokenKind == TokenKind.NewLine) {
            //    _atLineHead = true;
            //}
            //else if (tokenKind != TokenKind.Whitespace) {
            //    _atLineHead = false;
            //}
            //_atLineHead = false;
            return new Token((int)tokenKind, value, CreateFullTextSpan());
        }
        private TextSpan CreateSingleTextSpan() {
            var pos = new TextPosition(_line, _column);
            return new TextSpan(_filePath, _totalIndex, _index < _count ? 1 : 0, pos, pos);
        }
        private Token CreateTokenAndAdvanceChar(char ch) {
            var token = new Token(ch, null, CreateSingleTextSpan());
            AdvanceChar(false);
            return token;
        }
        private void ErrorAndThrow(string errMsg, TextSpan textSpan) {
            _context.AddDiag(DiagnosticSeverity.Error, (int)DiagnosticCode.Parsing, errMsg, textSpan);
            throw ParsingException.Instance;
        }
        private void ErrorAndThrow(string errMsg) {
            ErrorAndThrow(errMsg, CreateSingleTextSpan());
        }
        //private enum State : byte {
        //    None = 0,
        //    InPpName,
        //    InPpRegionComment,
        //    InPpFalseConditionBlock,
        //    //InWhitespace,
        //    //InNewLine,
        //    InSingleLineComment,
        //    InDelimitedComment,
        //    InNormalIdentifier,
        //    InVerbatimIdentifier,
        //    InNormalString,
        //    InVerbatimString,
        //    InChar,
        //    InNumberInteger,
        //    InNumberFraction,
        //    InNumberExponent,
        //}

        //public Token GetToken() {
        //    while (true) {
        //        var token = GetTokenCore();
        //        var tokenKind = token.TokenKind;
        //        if (tokenKind == TokenKind.Whitespace || tokenKind == TokenKind.NewLine ||
        //            tokenKind == TokenKind.SingleLineComment || tokenKind == TokenKind.MultiLineComment) {
        //            continue;
        //        }
        //        if (tokenKind != TokenKind.PpHash) {
        //            _getNonTrivalToken = true;
        //            return token;
        //        }
        //        token = GetNonWhitespaceToken();
        //        if (!token.IsNormalIdentifier) {
        //            ErrorAndThrow("Identifier expected.", token.TextSpan);
        //        }
        //        var ppDirective = token.Value;
        //        if (ppDirective == "region") {
        //            ++_ppRegionCount;
        //            GetTokenCore(State.InPpRegionComment);
        //            continue;
        //        }
        //        if (ppDirective == "endregion") {
        //            if (--_ppRegionCount < 0) {
        //                ErrorAndThrow("Unexpected #endregion.", token.TextSpan);
        //            }
        //            GetTokenCore(State.InPpRegionComment);
        //            continue;
        //        }
        //        if (ppDirective == "define") {
        //            if (_getNonTrivalToken) {
        //                ErrorAndThrow("Unexpected #define.", token.TextSpan);
        //            }
        //            PpSymbolSet.Add(ppDirective);
        //            continue;
        //        }
        //        if (ppDirective == "undef") {
        //            if (_getNonTrivalToken) {
        //                ErrorAndThrow("Unexpected #undef.", token.TextSpan);
        //            }
        //            PpSymbolSet.Remove(ppDirective);
        //            continue;
        //        }
        //        //
        //        _getNonTrivalToken = true;
        //        bool conditionValue;
        //        var stack = PpConditionStack;
        //        if (ppDirective == "if") {
        //            stack.Push(new PpCondition(PpConditionKind.If, conditionValue = PpExpression()));
        //        }
        //        else if (ppDirective == "elif") {
        //            if (stack.Count == 0 || stack.Peek().Kind == PpConditionKind.Else) {
        //                ErrorAndThrow("Unexpected #elif.", token.TextSpan);
        //            }
        //            stack.Pop();
        //            stack.Push(new PpCondition(PpConditionKind.Elif, conditionValue = PpExpression()));
        //        }
        //        else if (ppDirective == "else") {
        //            if (stack.Count == 0 || stack.Peek().Kind == PpConditionKind.Else) {
        //                ErrorAndThrow("Unexpected #else.", token.TextSpan);
        //            }
        //            stack.Push(new PpCondition(PpConditionKind.Else, conditionValue = !stack.Pop().Value));
        //        }
        //        else if (ppDirective == "endif") {
        //            if (stack.Count == 0) {
        //                ErrorAndThrow("Unexpected #endif.", token.TextSpan);
        //            }
        //            stack.Pop();
        //            conditionValue = true;
        //        }
        //        else {
        //            ErrorAndThrow("Invalid preprocessor directive.", token.TextSpan);
        //            conditionValue = false;
        //        }
        //        if (stack.Count > 0) {
        //            conditionValue = stack.Peek().Value && conditionValue;
        //        }
        //        if (_ppExprToken != null) {
        //            token = _ppExprToken.Value;
        //            _ppExprToken = null;
        //            if (token.IsWhitespace) {
        //                token = GetTokenCore();
        //            }
        //        }
        //        else {
        //            token = GetNonWhitespaceToken();
        //        }
        //        if (token.TokenKind == TokenKind.SingleLineComment) {
        //            token = GetTokenCore();
        //        }
        //        if (!token.IsNewLine || !token.IsEndOfFile) {
        //            ErrorAndThrow("Single-line comment or end-of-line expected.", token.TextSpan);
        //        }
        //        if (!conditionValue) {
        //            GetTokenCore(State.InPpFalseConditionBlock);
        //        }


        //    }
        //}
        //private Token GetNonWhitespaceToken() {
        //    while (true) {
        //        var token = GetTokenCore();
        //        if (!token.IsWhitespace) {
        //            return token;
        //        }
        //    }
        //}

        public Token GetToken() {
            char ch, nextch, nextnextch;
            StringBuilder sb;
            while (true) {
                ch = GetChar();
                switch (ch) {
                    case char.MaxValue:
                        return CreateTokenAndAdvanceChar(ch);
                    case ' ':
                    case '\t':
                        AdvanceChar(false);
                        break;
                    case '\r':
                    case '\n':
                        AdvanceChar(true);
                        break;
                    case 'a':
                    case 'b':
                    case 'c':
                    case 'd':
                    case 'e':
                    case 'f':
                    case 'g':
                    case 'h':
                    case 'i':
                    case 'j':
                    case 'k':
                    case 'l':
                    case 'm':
                    case 'n':
                    case 'o':
                    case 'p':
                    case 'q':
                    case 'r':
                    case 's':
                    case 't':
                    case 'u':
                    case 'v':
                    case 'w':
                    case 'x':
                    case 'y':
                    case 'z':
                    case 'A':
                    case 'B':
                    case 'C':
                    case 'D':
                    case 'E':
                    case 'F':
                    case 'G':
                    case 'H':
                    case 'I':
                    case 'J':
                    case 'K':
                    case 'L':
                    case 'M':
                    case 'N':
                    case 'O':
                    case 'P':
                    case 'Q':
                    case 'R':
                    case 'S':
                    case 'T':
                    case 'U':
                    case 'V':
                    case 'W':
                    case 'X':
                    case 'Y':
                    case 'Z':
                    case '_':
                        MarkTokenStart();
                        AdvanceChar(false);
                        return CreateIdToken(GetStringBuilder().Append(ch));
                    case '/':
                        nextch = GetNextChar();
                        if (nextch == '/') {
                            AdvanceChar(false);
                            AdvanceChar(false);
                            while (true) {
                                ch = GetChar();
                                if (ch == char.MaxValue || SyntaxFacts.IsNewLine(ch)) {
                                    break;
                                }
                                else
                                    AdvanceChar(false);
                            }
                        }
                        else if (nextch == '*') {
                            AdvanceChar(false);
                            AdvanceChar(false);
                            while (true) {
                                ch = GetChar();
                                if (ch == '*') {
                                    AdvanceChar(false);
                                    ch = GetChar();
                                    if (ch == '/') {
                                        AdvanceChar(false);
                                        break;
                                    }
                                }
                                else if (ch == char.MaxValue) {
                                    ErrorAndThrow("*/ expected.");
                                }
                                else {
                                    AdvanceChar(true);
                                }
                            }
                        }
                        else if (nextch == '=') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.SlashEquals);
                        }
                        else {
                            return CreateTokenAndAdvanceChar(ch);
                        }
                        break;
                    case '@':
                        nextch = GetNextChar();
                        if (nextch == '"') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            sb = GetStringBuilder();
                            while (true) {
                                ch = GetChar();
                                if (ch == '"') {
                                    AdvanceChar(false);
                                    ch = GetChar();
                                    if (ch == '"') {
                                        sb.Append('"');
                                        AdvanceChar(false);
                                    }
                                    else {
                                        return CreateToken(TokenKind.VerbatimString, sb.ToString());
                                    }
                                }
                                else if (ch == char.MaxValue) {
                                    ErrorAndThrow("\" expected.");
                                }
                                else {
                                    sb.Append(ch);
                                    AdvanceChar(true);
                                }
                            }
                        }
                        else if (SyntaxFacts.IsIdentifierStartCharacter(nextch)) {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateIdToken(GetStringBuilder().Append(nextch), TokenKind.VerbatimIdentifier);
                        }
                        else {
                            return CreateTokenAndAdvanceChar(ch);
                        }
                    case '"':
                        MarkTokenStart();
                        AdvanceChar(false);
                        sb = GetStringBuilder();
                        while (true) {
                            ch = GetChar();
                            if (ch == '\\') {
                                AdvanceChar(false);
                                CreateCharEscapeSequence(sb);
                            }
                            else if (ch == '"') {
                                AdvanceChar(false);
                                return CreateToken(TokenKind.NormalString, sb.ToString());
                            }
                            else if (ch == char.MaxValue || SyntaxFacts.IsNewLine(ch)) {
                                ErrorAndThrow("\" expected.");
                            }
                            else {
                                sb.Append(ch);
                                AdvanceChar(false);
                            }
                        }
                    case '\'':
                        MarkTokenStart();
                        AdvanceChar(false);
                        sb = GetStringBuilder();
                        while (true) {
                            ch = GetChar();
                            if (sb.Length == 0) {
                                if (ch == '\\') {
                                    AdvanceChar(false);
                                    CreateCharEscapeSequence(sb);
                                }
                                else if (ch == '\'' || ch == char.MaxValue || SyntaxFacts.IsNewLine(ch) ) {
                                    ErrorAndThrow("Character expected.");
                                }
                                else {
                                    sb.Append(ch);
                                    AdvanceChar(false);
                                }
                            }
                            else if (ch == '\'') {
                                AdvanceChar(false);
                                return CreateToken(TokenKind.Char, sb.ToString());
                            }
                            else {
                                ErrorAndThrow("' expected.");
                            }
                        }
                    case '|':
                        nextch = GetNextChar();
                        if (nextch == '|') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.BarBar);
                        }
                        else if (nextch == '=') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.BarEquals);
                        }
                        else {
                            return CreateTokenAndAdvanceChar(ch);
                        }
                    case '&':
                        nextch = GetNextChar();
                        if (nextch == '&') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.AmpersandAmpersand);
                        }
                        else if (nextch == '=') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.AmpersandEquals);
                        }
                        else {
                            return CreateTokenAndAdvanceChar(ch);
                        }
                    case '-':
                        nextch = GetNextChar();
                        if (nextch == '-') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.MinusMinus);
                        }
                        else if (nextch == '=') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.MinusEquals);
                        }
                        else if (nextch == '>') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.MinusGreaterThan);
                        }
                        else {
                            return CreateTokenAndAdvanceChar(ch);
                        }
                    case '+':
                        nextch = GetNextChar();
                        if (nextch == '+') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.PlusPlus);
                        }
                        else if (nextch == '=') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.PlusEquals);
                        }
                        else {
                            return CreateTokenAndAdvanceChar(ch);
                        }
                    case '!':
                        nextch = GetNextChar();
                        if (nextch == '=') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.ExclamationEquals);
                        }
                        else {
                            return CreateTokenAndAdvanceChar(ch);
                        }
                    case '=':
                        nextch = GetNextChar();
                        if (nextch == '=') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.EqualsEquals);
                        }
                        else if (nextch == '>') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.EqualsGreaterThan);
                        }
                        else {
                            return CreateTokenAndAdvanceChar(ch);
                        }
                    case '<':
                        nextch = GetNextChar();
                        if (nextch == '=') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.LessThanEquals);
                        }
                        else if (nextch == '<') {
                            nextnextch = GetNextNextChar();
                            if (nextnextch == '=') {
                                MarkTokenStart();
                                AdvanceChar(false);
                                AdvanceChar(false);
                                AdvanceChar(false);
                                return CreateToken(TokenKind.LessThanLessThanEquals);
                            }
                            else {
                                MarkTokenStart();
                                AdvanceChar(false);
                                AdvanceChar(false);
                                return CreateToken(TokenKind.LessThanLessThan);
                            }
                        }
                        else {
                            return CreateTokenAndAdvanceChar(ch);
                        }
                    case '>':
                        nextch = GetNextChar();
                        if (nextch == '=') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.GreaterThanEquals);
                        }
                        else {
                            return CreateTokenAndAdvanceChar(ch);
                        }
                    case '*':
                        nextch = GetNextChar();
                        if (nextch == '=') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.AsteriskEquals);
                        }
                        else {
                            return CreateTokenAndAdvanceChar(ch);
                        }
                    case '^':
                        nextch = GetNextChar();
                        if (nextch == '=') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.CaretEquals);
                        }
                        else {
                            return CreateTokenAndAdvanceChar(ch);
                        }
                    case '%':
                        nextch = GetNextChar();
                        if (nextch == '=') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.PercentEquals);
                        }
                        else {
                            return CreateTokenAndAdvanceChar(ch);
                        }
                    case '?':
                        nextch = GetNextChar();
                        if (nextch == '?') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.QuestionQuestion);
                        }
                        else {
                            return CreateTokenAndAdvanceChar(ch);
                        }
                    case ':':
                        nextch = GetNextChar();
                        if (nextch == ':') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.ColonColon);
                        }
                        else {
                            return CreateTokenAndAdvanceChar(ch);
                        }
                    case '#':
                        nextch = GetNextChar();
                        if (nextch == '#') {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.HashHash);
                        }
                        else if (_atLineHead) {
                            AdvanceChar(false);
                            //todo
                            break;
                        }
                        else {
                            return CreateTokenAndAdvanceChar(ch);
                        }


                    default:
                        if (SyntaxFacts.IsWhitespace(ch)) {
                            AdvanceChar(false);
                        }
                        else if (SyntaxFacts.IsNewLine(ch)) {
                            AdvanceChar(true);
                        }
                        else if (SyntaxFacts.IsIdentifierStartCharacter(ch)) {
                            MarkTokenStart();
                            AdvanceChar(false);
                            return CreateIdToken(GetStringBuilder().Append(ch));
                        }
                        else if (IsDecDigit(ch)) {
                            MarkTokenStart();
                            AdvanceChar(false);
                            sb = GetStringBuilder();
                            sb.Append(ch);
                            //todo
                        }
                        else {
                            return CreateTokenAndAdvanceChar(ch);
                        }

                        break;
                }
            }
        }
        //public Token GetToken() {
        //    StringBuilder sb = null;
        //    while (true) {
        //        var ch = GetChar();
        //                switch (ch) {
        //                    case char.MaxValue:
        //                        return CreateTokenAndAdvanceChar(ch);
        //                    case '/':
        //                        {
        //                            var nextch = GetNextChar();
        //                            if (nextch == '/') {
        //                                //MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);

        //                            }
        //                            else if (nextch == '*') {
        //                                state = State.InDelimitedComment;
        //                                //MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                            }
        //                            else if (nextch == '=') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.SlashEquals);
        //                            }
        //                            else {
        //                                return CreateTokenAndAdvanceChar(ch);
        //                            }
        //                        }
        //                        break;
        //                    case '@':
        //                        {
        //                            var nextch = GetNextChar();
        //                            if (nextch == '"') {
        //                                state = State.InVerbatimString;
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                sb = GetStringBuilder();
        //                            }
        //                            else if (SyntaxFacts.IsIdentifierStartCharacter(nextch)) {
        //                                state = State.InVerbatimIdentifier;
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                sb = GetStringBuilder();
        //                                sb.Append(nextch);
        //                            }
        //                            else {
        //                                return CreateTokenAndAdvanceChar(ch);
        //                            }
        //                        }
        //                        break;
        //                    case '"':
        //                        {
        //                            state = State.InNormalString;
        //                            MarkTokenStart();
        //                            AdvanceChar(false);
        //                            sb = GetStringBuilder();
        //                        }
        //                        break;
        //                    case '\'':
        //                        {
        //                            state = State.InChar;
        //                            MarkTokenStart();
        //                            AdvanceChar(false);
        //                            sb = GetStringBuilder();
        //                        }
        //                        break;
        //                    case '|':
        //                        {
        //                            var nextch = GetNextChar();
        //                            if (nextch == '|') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.BarBar);
        //                            }
        //                            else if (nextch == '=') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.BarEquals);
        //                            }
        //                            else {
        //                                return CreateTokenAndAdvanceChar(ch);
        //                            }
        //                        }
        //                    case '&':
        //                        {
        //                            var nextch = GetNextChar();
        //                            if (nextch == '&') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.AmpersandAmpersand);
        //                            }
        //                            else if (nextch == '=') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.AmpersandEquals);
        //                            }
        //                            else {
        //                                return CreateTokenAndAdvanceChar(ch);
        //                            }
        //                        }
        //                    case '-':
        //                        {
        //                            var nextch = GetNextChar();
        //                            if (nextch == '-') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.MinusMinus);
        //                            }
        //                            else if (nextch == '=') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.MinusEquals);
        //                            }
        //                            else if (nextch == '>') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.MinusGreaterThan);
        //                            }
        //                            else {
        //                                return CreateTokenAndAdvanceChar(ch);
        //                            }
        //                        }
        //                    case '+':
        //                        {
        //                            var nextch = GetNextChar();
        //                            if (nextch == '+') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.PlusPlus);
        //                            }
        //                            else if (nextch == '=') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.PlusEquals);
        //                            }
        //                            else {
        //                                return CreateTokenAndAdvanceChar(ch);
        //                            }
        //                        }
        //                    case '!':
        //                        {
        //                            var nextch = GetNextChar();
        //                            if (nextch == '=') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.ExclamationEquals);
        //                            }
        //                            else {
        //                                return CreateTokenAndAdvanceChar(ch);
        //                            }
        //                        }
        //                    case '=':
        //                        {
        //                            var nextch = GetNextChar();
        //                            if (nextch == '=') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.EqualsEquals);
        //                            }
        //                            else if (nextch == '>') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.EqualsGreaterThan);
        //                            }
        //                            else {
        //                                return CreateTokenAndAdvanceChar(ch);
        //                            }
        //                        }
        //                    case '<':
        //                        {
        //                            var nextch = GetNextChar();
        //                            if (nextch == '=') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.LessThanEquals);
        //                            }
        //                            else if (nextch == '<') {
        //                                var nextnextch = GetNextNextChar();
        //                                if (nextnextch == '=') {
        //                                    MarkTokenStart();
        //                                    AdvanceChar(false);
        //                                    AdvanceChar(false);
        //                                    AdvanceChar(false);
        //                                    return CreateToken(TokenKind.LessThanLessThanEquals);
        //                                }
        //                                else {
        //                                    MarkTokenStart();
        //                                    AdvanceChar(false);
        //                                    AdvanceChar(false);
        //                                    return CreateToken(TokenKind.LessThanLessThan);
        //                                }
        //                            }
        //                            else {
        //                                return CreateTokenAndAdvanceChar(ch);
        //                            }
        //                        }
        //                    case '>':
        //                        {
        //                            var nextch = GetNextChar();
        //                            if (nextch == '=') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.GreaterThanEquals);
        //                            }
        //                            else {
        //                                return CreateTokenAndAdvanceChar(ch);
        //                            }
        //                        }
        //                    case '*':
        //                        {
        //                            var nextch = GetNextChar();
        //                            if (nextch == '=') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.AsteriskEquals);
        //                            }
        //                            else {
        //                                return CreateTokenAndAdvanceChar(ch);
        //                            }
        //                        }
        //                    case '^':
        //                        {
        //                            var nextch = GetNextChar();
        //                            if (nextch == '=') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.CaretEquals);
        //                            }
        //                            else {
        //                                return CreateTokenAndAdvanceChar(ch);
        //                            }
        //                        }
        //                    case '%':
        //                        {
        //                            var nextch = GetNextChar();
        //                            if (nextch == '=') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.PercentEquals);
        //                            }
        //                            else {
        //                                return CreateTokenAndAdvanceChar(ch);
        //                            }
        //                        }
        //                    case '?':
        //                        {
        //                            var nextch = GetNextChar();
        //                            if (nextch == '?') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.QuestionQuestion);
        //                            }
        //                            else {
        //                                return CreateTokenAndAdvanceChar(ch);
        //                            }
        //                        }
        //                    case ':':
        //                        {
        //                            var nextch = GetNextChar();
        //                            if (nextch == ':') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.ColonColon, null);
        //                            }
        //                            else {
        //                                return CreateTokenAndAdvanceChar(ch);
        //                            }
        //                        }
        //                    case '#':
        //                        {
        //                            var nextch = GetNextChar();
        //                            if (nextch == '#') {
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                return CreateToken(TokenKind.HashHash, null);
        //                            }
        //                            else if (_atLineHead) {
        //                                AdvanceChar(false);
        //                                state = State.InPpName;
        //                            }
        //                            else {
        //                                return CreateTokenAndAdvanceChar(ch);
        //                            }
        //                        }
        //                        break;

        //                    //case '-':
        //                    //case '+':
        //                    //    {
        //                    //        var nextch = GetNextChar();
        //                    //        if (IsDecDigit(nextch)) {
        //                    //            state = State.InNumberInteger;
        //                    //            MarkTokenStart();
        //                    //            AdvanceChar(false);
        //                    //            AdvanceChar(false);
        //                    //            sb = GetStringBuilder();
        //                    //            sb.Append(ch);
        //                    //            sb.Append(nextch);
        //                    //        }
        //                    //        else if (nextch == '.') {
        //                    //            var nextnextch = GetNextNextChar();
        //                    //            if (IsDecDigit(nextnextch)) {
        //                    //                state = State.InNumberFraction;
        //                    //                MarkTokenStart();
        //                    //                AdvanceChar(false);
        //                    //                AdvanceChar(false);
        //                    //                AdvanceChar(false);
        //                    //                sb = GetStringBuilder();
        //                    //                sb.Append(ch);
        //                    //                sb.Append(nextch);
        //                    //                sb.Append(nextnextch);
        //                    //            }
        //                    //            else {
        //                    //                return CreateTokenAndAdvanceChar(ch);
        //                    //            }
        //                    //        }
        //                    //        else {
        //                    //            return CreateTokenAndAdvanceChar(ch);
        //                    //        }
        //                    //    }
        //                    //    break;
        //                    case '.':
        //                        {
        //                            var nextch = GetNextChar();
        //                            if (IsDecDigit(nextch)) {
        //                                state = State.InNumberFraction;
        //                                MarkTokenStart();
        //                                AdvanceChar(false);
        //                                AdvanceChar(false);
        //                                sb = GetStringBuilder();
        //                                sb.Append(ch);
        //                                sb.Append(nextch);
        //                            }
        //                            else {
        //                                return CreateTokenAndAdvanceChar(ch);
        //                            }
        //                        }
        //                        break;
        //                    default:
        //                        if (SyntaxFacts.IsWhitespace(ch)) {
        //                            AdvanceChar(false);
        //                        }
        //                        else if (SyntaxFacts.IsNewLine(ch)) {
        //                            AdvanceChar(true);
        //                            _atLineHead = true;
        //                        }
        //                        else if (SyntaxFacts.IsIdentifierStartCharacter(ch)) {
        //                            state = State.InNormalIdentifier;
        //                            MarkTokenStart();
        //                            AdvanceChar(false);
        //                            sb = GetStringBuilder();
        //                            sb.Append(ch);
        //                        }
        //                        else if (IsDecDigit(ch)) {
        //                            state = State.InNumberInteger;
        //                            MarkTokenStart();
        //                            AdvanceChar(false);
        //                            sb = GetStringBuilder();
        //                            sb.Append(ch);
        //                        }
        //                        else {
        //                            return CreateTokenAndAdvanceChar(ch);
        //                        }
        //                        break;
        //                }
        //                break;
        //            case State.InPpName:
        //                {
        //                    TextSpan ts;
        //                    var ppName = GetPpIdentifier(out ts);
        //                    switch (ppName) {
        //                        case "region":
        //                            ++_ppRegionCount;
        //                            break;
        //                        case "endregion":
        //                            if (--_ppRegionCount < 0) {
        //                                ErrorAndThrow("Unexpected #endregion.", ts);
        //                            }
        //                            break;
        //                        case "define":
        //                            if (_getNonTrivalToken) {
        //                                ErrorAndThrow("Unexpected #define.", ts);
        //                            }
        //                            PpSymbolSet.Add(GetPpIdentifier(out ts));
        //                            break;
        //                        case "undef":
        //                            if (_getNonTrivalToken) {
        //                                ErrorAndThrow("Unexpected #undef.", ts);
        //                            }
        //                            PpSymbolSet.Remove(GetPpIdentifier(out ts));
        //                            break;


        //                        default:
        //                            break;
        //                    }
        //                    _getNonTrivalToken = true;
        //                    bool conditionValue;
        //                    var stack = PpConditionStack;
        //                    if (ppName == "if") {
        //                        stack.Push(new PpCondition(PpConditionKind.If, conditionValue = PpExpression()));
        //                    }
        //                    else if (ppName == "elif") {
        //                        if (stack.Count == 0 || stack.Peek().Kind == PpConditionKind.Else) {
        //                            ErrorAndThrow("Unexpected #elif.", ts);
        //                        }
        //                        stack.Pop();
        //                        stack.Push(new PpCondition(PpConditionKind.Elif, conditionValue = PpExpression()));
        //                    }
        //                    else if (ppName == "else") {
        //                        if (stack.Count == 0 || stack.Peek().Kind == PpConditionKind.Else) {
        //                            ErrorAndThrow("Unexpected #else.", ts);
        //                        }
        //                        stack.Push(new PpCondition(PpConditionKind.Else, conditionValue = !stack.Pop().Value));
        //                    }
        //                    else if (ppName == "endif") {
        //                        if (stack.Count == 0) {
        //                            ErrorAndThrow("Unexpected #endif.", ts);
        //                        }
        //                        stack.Pop();
        //                        conditionValue = true;
        //                    }
        //                    else {
        //                        ErrorAndThrow("Invalid preprocessor directive.", ts);
        //                        conditionValue = false;
        //                    }
        //                    if (stack.Count > 0) {
        //                        conditionValue = stack.Peek().Value && conditionValue;
        //                    }


        //                }


        //                break;
        //            case State.InSingleLineComment:
        //                if (SyntaxFacts.IsNewLine(ch) || ch == char.MaxValue) {
        //                    state = State.None;
        //                }
        //                else {
        //                    AdvanceChar(false);
        //                }
        //                break;
        //            case State.InDelimitedComment:
        //                if (ch == '*') {
        //                    AdvanceChar(false);
        //                    ch = GetChar();
        //                    if (ch == '/') {
        //                        AdvanceChar(false);
        //                        state = State.None;
        //                    }
        //                }
        //                else if (ch == char.MaxValue) {
        //                    ErrorAndThrow("*/ expected.");
        //                }
        //                else {
        //                    AdvanceChar(true);
        //                }
        //                break;
        //            case State.InNormalIdentifier:
        //            case State.InVerbatimIdentifier:
        //                if (SyntaxFacts.IsIdentifierPartCharacter(ch)) {
        //                    sb.Append(ch);
        //                    AdvanceChar(false);
        //                }
        //                else {
        //                    return CreateToken(state == State.InNormalIdentifier ? TokenKind.NormalIdentifier : TokenKind.VerbatimIdentifier, sb.ToString());
        //                }
        //                break;
        //            case State.InNormalString:
        //                if (ch == '\\') {
        //                    AdvanceChar(false);
        //                    ProcessCharEscSeq(sb);
        //                }
        //                else if (ch == '"') {
        //                    AdvanceChar(false);
        //                    return CreateToken(TokenKind.NormalString, sb.ToString());
        //                }
        //                else if (SyntaxFacts.IsNewLine(ch) || ch == char.MaxValue) {
        //                    ErrorAndThrow("\" expected.");
        //                }
        //                else {
        //                    sb.Append(ch);
        //                    AdvanceChar(false);
        //                }
        //                break;
        //            case State.InVerbatimString:
        //                if (ch == '"') {
        //                    AdvanceChar(false);
        //                    ch = GetChar();
        //                    if (ch == '"') {
        //                        sb.Append('"');
        //                        AdvanceChar(false);
        //                    }
        //                    else {
        //                        return CreateToken(TokenKind.VerbatimString, sb.ToString());
        //                    }
        //                }
        //                else if (ch == char.MaxValue) {
        //                    ErrorAndThrow("\" expected.");
        //                }
        //                else {
        //                    sb.Append(ch);
        //                    AdvanceChar(true);
        //                }
        //                break;
        //            case State.InChar:
        //                if (sb.Length == 0) {
        //                    if (ch == '\\') {
        //                        AdvanceChar(false);
        //                        ProcessCharEscSeq(sb);
        //                    }
        //                    else if (ch == '\'' || SyntaxFacts.IsNewLine(ch) || ch == char.MaxValue) {
        //                        ErrorAndThrow("Character expected.");
        //                    }
        //                    else {
        //                        sb.Append(ch);
        //                        AdvanceChar(false);
        //                    }
        //                }
        //                else if (ch == '\'') {
        //                    AdvanceChar(false);
        //                    return CreateToken(TokenKind.Char, sb.ToString());
        //                }
        //                else {
        //                    ErrorAndThrow("' expected.");
        //                }
        //                break;


        //            default:
        //                throw new InvalidOperationException("Invalid state: " + state);

        //        }

        //        //
        //        //del
        //        //if (state == State.InWhitespace) {
        //        //    if (SyntaxFacts.IsWhitespace(ch)) {
        //        //        AdvanceChar();
        //        //    }
        //        //    else {
        //        //        return CreateToken(TokenKind.Whitespace);
        //        //    }
        //        //}
        //        //else if (state == State.InNewLine) {
        //        //    if (SyntaxFacts.IsNewLine(ch)) {
        //        //        AdvanceChar();
        //        //    }
        //        //    else {
        //        //        return CreateToken(TokenKind.NewLine);
        //        //    }
        //        //}
        //        //else if (state == State.InSingleLineComment) {
        //        //    if (SyntaxFacts.IsNewLine(ch) || ch == char.MaxValue) {
        //        //        return CreateToken(TokenKind.SingleLineComment);
        //        //    }
        //        //    else {
        //        //        AdvanceChar();
        //        //    }
        //        //}
        //        //else if (state == State.InPpRegionComment) {
        //        //    if (SyntaxFacts.IsNewLine(ch) || ch == char.MaxValue) {
        //        //        return default(Token);
        //        //    }
        //        //    else {
        //        //        AdvanceChar();
        //        //    }
        //        //}
        //        //else if (state == State.InPpFalseConditionBlock) {
        //        //    if (ch == '#' && _atLineHead || ch == char.MaxValue) {
        //        //        return default(Token);
        //        //    }
        //        //    if (SyntaxFacts.IsNewLine(ch)) {
        //        //        _atLineHead = true;
        //        //    }
        //        //    else if (!SyntaxFacts.IsWhitespace(ch)) {
        //        //        _atLineHead = false;
        //        //    }
        //        //    AdvanceChar();
        //        //}
        //        //else if (state == State.InDelimitedComment) {
        //        //    if (ch == '*') {
        //        //        AdvanceChar();
        //        //        ch = GetChar();
        //        //        if (ch == '/') {
        //        //            AdvanceChar();
        //        //            return CreateToken(TokenKind.MultiLineComment);
        //        //        }
        //        //    }
        //        //    else if (ch == char.MaxValue) {
        //        //        ErrorAndThrow("*/ expected.");
        //        //    }
        //        //    else {
        //        //        AdvanceChar();
        //        //    }
        //        //}
        //        //else if (state == State.InNormalIdentifier || state == State.InVerbatimIdentifier) {
        //        //    if (SyntaxFacts.IsIdentifierPartCharacter(ch)) {
        //        //        sb.Append(ch);
        //        //        AdvanceChar();
        //        //    }
        //        //    else {
        //        //        return CreateToken(state == State.InNormalIdentifier ? TokenKind.NormalIdentifier : TokenKind.VerbatimIdentifier, sb.ToString());
        //        //    }
        //        //}
        //        //else if (state == State.InNormalString) {
        //        //    if (ch == '\\') {
        //        //        AdvanceChar();
        //        //        ProcessCharEscSeq(sb);
        //        //    }
        //        //    else if (ch == '"') {
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.NormalString, sb.ToString());
        //        //    }
        //        //    else if (SyntaxFacts.IsNewLine(ch) || ch == char.MaxValue) {
        //        //        ErrorAndThrow("\" expected.");
        //        //    }
        //        //    else {
        //        //        sb.Append(ch);
        //        //        AdvanceChar();
        //        //    }
        //        //}
        //        //else if (state == State.InVerbatimString) {
        //        //    if (ch == '"') {
        //        //        AdvanceChar();
        //        //        ch = GetChar();
        //        //        if (ch == '"') {
        //        //            sb.Append('"');
        //        //            AdvanceChar();
        //        //        }
        //        //        else {
        //        //            return CreateToken(TokenKind.VerbatimString, sb.ToString());
        //        //        }
        //        //    }
        //        //    else if (ch == char.MaxValue) {
        //        //        ErrorAndThrow("\" expected.");
        //        //    }
        //        //    else {
        //        //        sb.Append(ch);
        //        //        AdvanceChar();
        //        //    }
        //        //}
        //        //else if (state == State.InChar) {
        //        //    if (ch == '\\') {
        //        //        AdvanceChar();
        //        //        ProcessCharEscSeq(sb);
        //        //    }
        //        //    else if (ch == '\'') {
        //        //        if (sb.Length == 1) {
        //        //            AdvanceChar();
        //        //            return CreateToken(TokenKind.Char, sb.ToString());
        //        //        }
        //        //        else {
        //        //            ErrorAndThrow("Character expected.");
        //        //        }
        //        //    }
        //        //    else if (SyntaxFacts.IsNewLine(ch) || ch == char.MaxValue) {
        //        //        ErrorAndThrow("' expected.");
        //        //    }
        //        //    else {
        //        //        if (sb.Length == 0) {
        //        //            sb.Append(ch);
        //        //            AdvanceChar();
        //        //        }
        //        //        else {
        //        //            ErrorAndThrow("' expected.");
        //        //        }
        //        //    }
        //        //}
        //        //else if (state == State.InNumberInteger) {
        //        //    if (IsDecDigit(ch)) {
        //        //        sb.Append(ch);
        //        //        AdvanceChar();
        //        //    }
        //        //    else if (ch == '.') {
        //        //        var nextch = GetNextChar();
        //        //        if (IsDecDigit(nextch)) {
        //        //            state = State.InNumberFraction;
        //        //            sb.Append(ch);
        //        //            sb.Append(nextch);
        //        //            AdvanceChar();
        //        //            AdvanceChar();
        //        //        }
        //        //        else {
        //        //            return CreateToken(TokenKind.IntegerLiteral, sb.ToString());
        //        //        }
        //        //    }
        //        //    else if (ch == 'E' || ch == 'e') {
        //        //        sb.Append(ch);
        //        //        AdvanceChar();
        //        //        ch = GetChar();
        //        //        if (ch == '+' || ch == '-') {
        //        //            sb.Append(ch);
        //        //            AdvanceChar();
        //        //            ch = GetChar();
        //        //        }
        //        //        if (IsDecDigit(ch)) {
        //        //            state = State.InNumberExponent;
        //        //            sb.Append(ch);
        //        //            AdvanceChar();
        //        //        }
        //        //        else {
        //        //            ErrorAndThrow("Decimal digit expected.");
        //        //        }
        //        //    }
        //        //    else {
        //        //        return CreateToken(TokenKind.IntegerLiteral, sb.ToString());
        //        //    }
        //        //}
        //        //else if (state == State.InNumberFraction) {
        //        //    if (IsDecDigit(ch)) {
        //        //        sb.Append(ch);
        //        //        AdvanceChar();
        //        //    }
        //        //    else if (ch == 'E' || ch == 'e') {
        //        //        sb.Append(ch);
        //        //        AdvanceChar();
        //        //        ch = GetChar();
        //        //        if (ch == '+' || ch == '-') {
        //        //            sb.Append(ch);
        //        //            AdvanceChar();
        //        //            ch = GetChar();
        //        //        }
        //        //        if (IsDecDigit(ch)) {
        //        //            state = State.InNumberExponent;
        //        //            sb.Append(ch);
        //        //            AdvanceChar();
        //        //        }
        //        //        else {
        //        //            ErrorAndThrow("Decimal digit expected.");
        //        //        }
        //        //    }
        //        //    else {
        //        //        return CreateToken(TokenKind.DecimalLiteral, sb.ToString());
        //        //    }
        //        //}
        //        //else if (state == State.InNumberExponent) {
        //        //    if (IsDecDigit(ch)) {
        //        //        sb.Append(ch);
        //        //        AdvanceChar();
        //        //    }
        //        //    else {
        //        //        return CreateToken(TokenKind.RealLiteral, sb.ToString());
        //        //    }
        //        //}
        //        ////
        //        ////
        //        ////
        //        //else if (ch == char.MaxValue) {
        //        //    return CreateTokenAndAdvanceChar(ch);
        //        //}
        //        //else if (SyntaxFacts.IsWhitespace(ch)) {
        //        //    state = State.InWhitespace;
        //        //    MarkTokenStart();
        //        //    AdvanceChar();
        //        //}
        //        //else if (SyntaxFacts.IsNewLine(ch)) {
        //        //    state = State.InNewLine;
        //        //    MarkTokenStart();
        //        //    AdvanceChar();
        //        //}
        //        //else if (ch == '/') {
        //        //    var nextch = GetNextChar();
        //        //    if (nextch == '/') {
        //        //        state = State.InSingleLineComment;
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //    }
        //        //    else if (nextch == '*') {
        //        //        state = State.InDelimitedComment;
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //    }
        //        //    else if (nextch == '=') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.SlashEquals);
        //        //    }
        //        //    else {
        //        //        return CreateTokenAndAdvanceChar(ch);
        //        //    }
        //        //}
        //        //else if (ch == '@') {
        //        //    var nextch = GetNextChar();
        //        //    if (nextch == '"') {
        //        //        state = State.InVerbatimString;
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        sb = GetStringBuilder();
        //        //    }
        //        //    else if (SyntaxFacts.IsIdentifierStartCharacter(nextch)) {
        //        //        state = State.InVerbatimIdentifier;
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        sb = GetStringBuilder();
        //        //        sb.Append(nextch);
        //        //    }
        //        //    else {
        //        //        return CreateTokenAndAdvanceChar(ch);
        //        //    }
        //        //}
        //        //else if (SyntaxFacts.IsIdentifierStartCharacter(ch)) {
        //        //    state = State.InNormalIdentifier;
        //        //    MarkTokenStart();
        //        //    AdvanceChar();
        //        //    sb = GetStringBuilder();
        //        //    sb.Append(ch);
        //        //}
        //        //else if (ch == '"') {
        //        //    state = State.InNormalString;
        //        //    MarkTokenStart();
        //        //    AdvanceChar();
        //        //    sb = GetStringBuilder();
        //        //}
        //        //else if (ch == '\'') {
        //        //    state = State.InChar;
        //        //    MarkTokenStart();
        //        //    AdvanceChar();
        //        //    sb = GetStringBuilder();
        //        //}
        //        //else if (IsDecDigit(ch)) {
        //        //    state = State.InNumberInteger;
        //        //    MarkTokenStart();
        //        //    AdvanceChar();
        //        //    sb = GetStringBuilder();
        //        //    sb.Append(ch);
        //        //}
        //        //else if (ch == '.') {
        //        //    var nextch = GetNextChar();
        //        //    if (IsDecDigit(nextch)) {
        //        //        state = State.InNumberFraction;
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        sb = GetStringBuilder();
        //        //        sb.Append(ch);
        //        //        sb.Append(nextch);
        //        //    }
        //        //    else {
        //        //        return CreateTokenAndAdvanceChar(ch);
        //        //    }
        //        //}
        //        //else if (ch == '|') {
        //        //    var nextch = GetNextChar();
        //        //    if (nextch == '|') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.BarBar);
        //        //    }
        //        //    else if (nextch == '=') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.BarEquals);
        //        //    }
        //        //    else {
        //        //        return CreateTokenAndAdvanceChar(ch);
        //        //    }
        //        //}
        //        //else if (ch == '&') {
        //        //    var nextch = GetNextChar();
        //        //    if (nextch == '&') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.AmpersandAmpersand);
        //        //    }
        //        //    else if (nextch == '=') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.AmpersandEquals);
        //        //    }
        //        //    else {
        //        //        return CreateTokenAndAdvanceChar(ch);
        //        //    }
        //        //}
        //        //else if (ch == '-') {
        //        //    var nextch = GetNextChar();
        //        //    if (nextch == '-') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.MinusMinus);
        //        //    }
        //        //    else if (nextch == '=') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.MinusEquals);
        //        //    }
        //        //    else if (nextch == '>') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.MinusGreaterThan);
        //        //    }
        //        //    else {
        //        //        return CreateTokenAndAdvanceChar(ch);
        //        //    }
        //        //}
        //        //else if (ch == '+') {
        //        //    var nextch = GetNextChar();
        //        //    if (nextch == '+') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.PlusPlus);
        //        //    }
        //        //    else if (nextch == '=') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.PlusEquals);
        //        //    }
        //        //    else {
        //        //        return CreateTokenAndAdvanceChar(ch);
        //        //    }
        //        //}
        //        //else if (ch == '!') {
        //        //    var nextch = GetNextChar();
        //        //    if (nextch == '=') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.ExclamationEquals);
        //        //    }
        //        //    else {
        //        //        return CreateTokenAndAdvanceChar(ch);
        //        //    }
        //        //}
        //        //else if (ch == '=') {
        //        //    var nextch = GetNextChar();
        //        //    if (nextch == '=') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.EqualsEquals);
        //        //    }
        //        //    else if (nextch == '>') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.EqualsGreaterThan);
        //        //    }
        //        //    else {
        //        //        return CreateTokenAndAdvanceChar(ch);
        //        //    }
        //        //}
        //        //else if (ch == '<') {
        //        //    var nextch = GetNextChar();
        //        //    if (nextch == '=') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.LessThanEquals);
        //        //    }
        //        //    else if (nextch == '<') {
        //        //        var nextnextch = GetNextNextChar();
        //        //        if (nextnextch == '=') {
        //        //            MarkTokenStart();
        //        //            AdvanceChar();
        //        //            AdvanceChar();
        //        //            AdvanceChar();
        //        //            return CreateToken(TokenKind.LessThanLessThanEquals);
        //        //        }
        //        //        else {
        //        //            MarkTokenStart();
        //        //            AdvanceChar();
        //        //            AdvanceChar();
        //        //            return CreateToken(TokenKind.LessThanLessThan);
        //        //        }
        //        //    }
        //        //    else {
        //        //        return CreateTokenAndAdvanceChar(ch);
        //        //    }
        //        //}
        //        //else if (ch == '>') {
        //        //    var nextch = GetNextChar();
        //        //    if (nextch == '=') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.GreaterThanEquals);
        //        //    }
        //        //    else {
        //        //        return CreateTokenAndAdvanceChar(ch);
        //        //    }
        //        //}
        //        //else if (ch == '*') {
        //        //    var nextch = GetNextChar();
        //        //    if (nextch == '=') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.AsteriskEquals);
        //        //    }
        //        //    else {
        //        //        return CreateTokenAndAdvanceChar(ch);
        //        //    }
        //        //}
        //        //else if (ch == '^') {
        //        //    var nextch = GetNextChar();
        //        //    if (nextch == '=') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.CaretEquals);
        //        //    }
        //        //    else {
        //        //        return CreateTokenAndAdvanceChar(ch);
        //        //    }
        //        //}
        //        //else if (ch == '%') {
        //        //    var nextch = GetNextChar();
        //        //    if (nextch == '=') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.PercentEquals);
        //        //    }
        //        //    else {
        //        //        return CreateTokenAndAdvanceChar(ch);
        //        //    }
        //        //}
        //        //else if (ch == '?') {
        //        //    var nextch = GetNextChar();
        //        //    if (nextch == '?') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.QuestionQuestion);
        //        //    }
        //        //    else {
        //        //        return CreateTokenAndAdvanceChar(ch);
        //        //    }
        //        //}
        //        //else if (ch == ':') {
        //        //    var nextch = GetNextChar();
        //        //    if (nextch == ':') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.ColonColon);
        //        //    }
        //        //    else {
        //        //        return CreateTokenAndAdvanceChar(ch);
        //        //    }
        //        //}
        //        //else if (ch == '#') {
        //        //    var nextch = GetNextChar();
        //        //    if (nextch == '#') {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.HashHash);
        //        //    }
        //        //    else if (_atLineHead) {
        //        //        MarkTokenStart();
        //        //        AdvanceChar();
        //        //        return CreateToken(TokenKind.PpHash);
        //        //    }
        //        //    else {
        //        //        return CreateTokenAndAdvanceChar(ch);
        //        //    }
        //        //}


        //        //else {
        //        //    return CreateTokenAndAdvanceChar(ch);
        //        //}
        //    }
        //}
        private Token CreateIdToken(StringBuilder sb, TokenKind kind = TokenKind.NormalIdentifier) {
            while (true) {
                var ch = GetChar();
                if (SyntaxFacts.IsIdentifierPartCharacter(ch)) {
                    sb.Append(ch);
                    AdvanceChar(false);
                }
                else {
                    return CreateToken(kind, sb.ToString());
                }
            }
        }
        private void CreateCharEscapeSequence(StringBuilder sb) {
            var ch = GetChar();
            switch (ch) {
                case 'u':
                    {
                        AdvanceChar(false);
                        int value = 0;
                        for (var i = 0; i < 4; ++i) {
                            ch = GetChar();
                            if (IsHexDigit(ch)) {
                                value <<= 4;
                                value |= HexValue(ch);
                                AdvanceChar(false);
                            }
                            else {
                                ErrorAndThrow("Invalid character escape sequence.");
                            }
                        }
                        sb.Append((char)value);
                    }
                    return;
                case '\'': sb.Append('\''); break;
                case '"': sb.Append('"'); break;
                case '\\': sb.Append('\\'); break;
                case '0': sb.Append('\0'); break;
                case 'a': sb.Append('\a'); break;
                case 'b': sb.Append('\b'); break;
                case 'f': sb.Append('\f'); break;
                case 'n': sb.Append('\n'); break;
                case 'r': sb.Append('\r'); break;
                case 't': sb.Append('\t'); break;
                case 'v': sb.Append('\v'); break;
                default: ErrorAndThrow("Invalid character escape sequence."); break;
            }
            AdvanceChar(false);
        }
        private enum PpConditionKind : byte {
            If,
            Elif,
            Else,
            Endif
        }
        private struct PpCondition {
            internal PpCondition(PpConditionKind kind, bool value) {
                Kind = kind;
                Value = value;
            }
            internal readonly PpConditionKind Kind;
            internal readonly bool Value;
        }
        private string TryGetPpIdentifier(out TextSpan ts) {
            var ch = GetChar();
            while (SyntaxFacts.IsWhitespace(ch)) {
                AdvanceChar(false);
                ch = GetChar();
            }
            if (SyntaxFacts.IsIdentifierStartCharacter(ch)) {
                var sb = GetStringBuilder();
                sb.Append(ch);
                MarkTokenStart();
                AdvanceChar(false);
                ch = GetChar();
                while (SyntaxFacts.IsIdentifierPartCharacter(ch)) {
                    sb.Append(ch);
                    AdvanceChar(false);
                    ch = GetChar();
                }
                ts = CreateFullTextSpan();
                return sb.ToString();
            }
            ts = default(TextSpan);
            return null;
        }
        private string GetPpIdentifier(out TextSpan ts) {
            var s = TryGetPpIdentifier(out ts);
            if (s == null) {
                ErrorAndThrow("Identifier expected.");
            }
            return s;
        }
        private bool PpExpression() {
            var result = PpAndExpression();
            while (true) {
                if (GetPpExprToken().TokenKind == TokenKind.BarBar) {
                    ConsumePpExprToken();
                    result = result || PpAndExpression();
                }
                else {
                    break;
                }
            }
            return result;
        }
        private bool PpAndExpression() {
            var result = PpEqualityExpression();
            while (true) {
                if (GetPpExprToken().TokenKind == TokenKind.AmpersandAmpersand) {
                    ConsumePpExprToken();
                    result = result && PpEqualityExpression();
                }
                else {
                    break;
                }
            }
            return result;
        }
        private bool PpEqualityExpression() {
            var result = PpUnaryExpression();
            while (true) {
                var tokenKind = GetPpExprToken().TokenKind;
                if (tokenKind == TokenKind.EqualsEquals) {
                    ConsumePpExprToken();
                    result = result == PpUnaryExpression();
                }
                else if (tokenKind == TokenKind.ExclamationEquals) {
                    ConsumePpExprToken();
                    result = result != PpUnaryExpression();
                }
                else {
                    break;
                }
            }
            return result;
        }
        private bool PpUnaryExpression() {
            if (GetPpExprToken().Kind == '!') {
                ConsumePpExprToken();
                return !PpUnaryExpression();
            }
            return PpPrimaryExpression();
        }
        private bool PpPrimaryExpression() {
            var token = GetPpExprToken();
            var tokenKind = token.Kind;
            if (tokenKind == (int)TokenKind.NormalIdentifier) {
                ConsumePpExprToken();
                var text = token.Value;
                if (text == "true") {
                    return true;
                }
                if (text == "false") {
                    return false;
                }
                return PpSymbolSet.Contains(text);
            }
            if (tokenKind == '(') {
                ConsumePpExprToken();
                var result = PpExpression();
                token = GetPpExprToken();
                if (token.Kind != ')') {
                    ErrorAndThrow(") expected.", token.TextSpan);
                }
                ConsumePpExprToken();
                return result;
            }
            ErrorAndThrow("Identifier expected.", token.TextSpan);
            return false;
        }
        private Token? _ppExprToken;
        private Token GetPpExprToken() {
            return (_ppExprToken ?? (_ppExprToken = GetPpExprTokenCore())).Value;
        }
        private void ConsumePpExprToken() {
            _ppExprToken = null;
        }
        private Token GetPpExprTokenCore() {
            TextSpan ts;
            var s = TryGetPpIdentifier(out ts);
            if (s != null) {
                return new Token((int)TokenKind.NormalIdentifier, s, ts);
            }
            var ch = GetChar();
            if (ch == '|') {
                var nextch = GetNextChar();
                if (nextch == '|') {
                    MarkTokenStart();
                    AdvanceChar(false);
                    AdvanceChar(false);
                    return CreateToken(TokenKind.BarBar);
                }
            }
            else if (ch == '&') {
                var nextch = GetNextChar();
                if (nextch == '&') {
                    MarkTokenStart();
                    AdvanceChar(false);
                    AdvanceChar(false);
                    return CreateToken(TokenKind.AmpersandAmpersand);
                }
            }
            else if (ch == '=') {
                var nextch = GetNextChar();
                if (nextch == '=') {
                    MarkTokenStart();
                    AdvanceChar(false);
                    AdvanceChar(false);
                    return CreateToken(TokenKind.EqualsEquals);
                }
            }
            else if (ch == '!') {
                var nextch = GetNextChar();
                if (nextch == '=') {
                    MarkTokenStart();
                    AdvanceChar(false);
                    AdvanceChar(false);
                    return CreateToken(TokenKind.ExclamationEquals);
                }
            }
            else if (ch == '/') {
                var nextch = GetNextChar();
                if (nextch == '/') {
                    MarkTokenStart();
                    AdvanceChar(false);
                    AdvanceChar(false);
                    while (true) {
                        ch = GetChar();
                        if (SyntaxFacts.IsNewLine(ch) || ch == char.MaxValue) {
                            break;
                        }
                        AdvanceChar(false);
                    }
                    return CreateToken(TokenKind.SingleLineComment);
                }
            }
            var tk = new Token(ch, null, CreateSingleTextSpan());
            AdvanceChar(true);
            return tk;
        }


        #region helpers
        //private static bool IsNewLine(char ch) {
        //    return ch == '\r'
        //        || ch == '\n'
        //        || ch == '\u0085'
        //        || ch == '\u2028'
        //        || ch == '\u2029';
        //}
        //private static bool IsWhitespace(char ch) {
        //    return ch == ' '
        //        || ch == '\t'
        //        || ch == '\v'
        //        || ch == '\f'
        //        || ch == '\u00A0'
        //        || ch == '\uFEFF'
        //        || ch == '\u001A'
        //        || (ch > 255 && CharUnicodeInfo.GetUnicodeCategory(ch) == UnicodeCategory.SpaceSeparator);
        //}
        private static bool IsDecDigit(char ch) {
            return ch >= '0' && ch <= '9';
        }
        private static bool IsHexDigit(char ch) {
            return (ch >= '0' && ch <= '9') ||
                   (ch >= 'A' && ch <= 'F') ||
                   (ch >= 'a' && ch <= 'f');
        }
        private static int DecValue(char ch) {
            return ch - '0';
        }
        private static int HexValue(char ch) {
            return (ch >= '0' && ch <= '9') ? ch - '0' : (ch & 0xdf) - 'A' + 10;
        }

        //public static bool IsIdentifierStartCharacter(char ch) {
        //    // identifier-start-character:
        //    //   letter-character
        //    //   _ (the underscore character U+005F)

        //    if (ch < 'a') // '\u0061'
        //    {
        //        if (ch < 'A') // '\u0041'
        //        {
        //            return false;
        //        }

        //        return ch <= 'Z'  // '\u005A'
        //            || ch == '_'; // '\u005F'
        //    }

        //    if (ch <= 'z') // '\u007A'
        //    {
        //        return true;
        //    }

        //    if (ch <= '\u007F') // max ASCII
        //    {
        //        return false;
        //    }

        //    return IsLetterChar(CharUnicodeInfo.GetUnicodeCategory(ch));
        //}
        //public static bool IsIdentifierPartCharacter(char ch) {
        //    // identifier-part-character:
        //    //   letter-character
        //    //   decimal-digit-character
        //    //   connecting-character
        //    //   combining-character
        //    //   formatting-character

        //    if (ch < 'a') // '\u0061'
        //    {
        //        if (ch < 'A') // '\u0041'
        //        {
        //            return ch >= '0'  // '\u0030'
        //                && ch <= '9'; // '\u0039'
        //        }

        //        return ch <= 'Z'  // '\u005A'
        //            || ch == '_'; // '\u005F'
        //    }

        //    if (ch <= 'z') // '\u007A'
        //    {
        //        return true;
        //    }

        //    if (ch <= '\u007F') // max ASCII
        //    {
        //        return false;
        //    }

        //    UnicodeCategory cat = CharUnicodeInfo.GetUnicodeCategory(ch);
        //    return IsLetterChar(cat)
        //        || IsDecimalDigitChar(cat)
        //        || IsConnectingChar(cat)
        //        || IsCombiningChar(cat)
        //        || IsFormattingChar(cat);
        //}
        //private static bool IsLetterChar(UnicodeCategory cat) {
        //    // letter-character:
        //    //   A Unicode character of classes Lu, Ll, Lt, Lm, Lo, or Nl 
        //    //   A Unicode-escape-sequence representing a character of classes Lu, Ll, Lt, Lm, Lo, or Nl

        //    switch (cat) {
        //        case UnicodeCategory.UppercaseLetter:
        //        case UnicodeCategory.LowercaseLetter:
        //        case UnicodeCategory.TitlecaseLetter:
        //        case UnicodeCategory.ModifierLetter:
        //        case UnicodeCategory.OtherLetter:
        //        case UnicodeCategory.LetterNumber:
        //            return true;
        //    }

        //    return false;
        //}
        //private static bool IsCombiningChar(UnicodeCategory cat) {
        //    // combining-character:
        //    //   A Unicode character of classes Mn or Mc 
        //    //   A Unicode-escape-sequence representing a character of classes Mn or Mc

        //    switch (cat) {
        //        case UnicodeCategory.NonSpacingMark:
        //        case UnicodeCategory.SpacingCombiningMark:
        //            return true;
        //    }

        //    return false;
        //}
        //private static bool IsDecimalDigitChar(UnicodeCategory cat) {
        //    // decimal-digit-character:
        //    //   A Unicode character of the class Nd 
        //    //   A unicode-escape-sequence representing a character of the class Nd

        //    return cat == UnicodeCategory.DecimalDigitNumber;
        //}
        //private static bool IsConnectingChar(UnicodeCategory cat) {
        //    // connecting-character:  
        //    //   A Unicode character of the class Pc
        //    //   A unicode-escape-sequence representing a character of the class Pc

        //    return cat == UnicodeCategory.ConnectorPunctuation;
        //}
        //private static bool IsFormattingChar(UnicodeCategory cat) {
        //    // formatting-character:  
        //    //   A Unicode character of the class Cf
        //    //   A unicode-escape-sequence representing a character of the class Cf

        //    return cat == UnicodeCategory.Format;
        //}
        #endregion helpers
    }


}

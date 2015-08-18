using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using Microsoft.CodeAnalysis.CSharp;

namespace CSharpParser
{
    public sealed class Lexer
    {
        [ThreadStatic]
        private static Lexer _instance;
        private static Lexer Instance
        {
            get { return _instance ?? (_instance = new Lexer()); }
        }
        public static Lexer Get(string filePath, TextReader reader, ParsingContext context, IEnumerable<string> ppSymbols)
        {
            return Instance.Init(filePath, reader, context, ppSymbols);
        }
        private Lexer()
        {
            _buf = new char[_bufLength];
            _ppSymbolSet = new HashSet<string>();
            _ppConditionStack = new Stack<PpCondition>();
        }
        //inputs
        private string _filePath;
        private TextReader _reader;
        private ParsingContext _context;
        //
        private const int _bufLength = 1024;
        private readonly char[] _buf;
        private int _index, _count;
        private bool _isEOF;
        private int _totalIndex;
        private int _lastLine, _lastColumn, _line, _column;
        private const int _stringBuilderCapacity = 256;
        private StringBuilder _stringBuilder;
        private bool _atLineHead;//whitespace is allowed 
        private bool _gotNonTrivalToken;
        private int _ppRegionCount;
        private readonly HashSet<string> _ppSymbolSet;
        private readonly Stack<PpCondition> _ppConditionStack;
        private Token? _ppExprToken;

        private Lexer Init(string filePath, TextReader reader, ParsingContext context, IEnumerable<string> ppSymbols)
        {
            if (filePath == null) throw new ArgumentNullException("filePath");
            if (reader == null) throw new ArgumentNullException("reader");
            if (context == null) throw new ArgumentNullException("context");
            _filePath = filePath;
            _reader = reader;
            _context = context;
            _index = _count = 0;
            _isEOF = false;
            _totalIndex = 0;
            _lastLine = _lastColumn = _line = _column = 1;
            if (_stringBuilder == null)
            {
                _stringBuilder = new StringBuilder(_stringBuilderCapacity);
            }
            _atLineHead = true;
            _gotNonTrivalToken = false;
            _ppRegionCount = 0;
            _ppSymbolSet.Clear();
            if (ppSymbols != null)
            {
                _ppSymbolSet.UnionWith(ppSymbols);
            }
            _ppConditionStack.Clear();
            _ppExprToken = null;
            return this;
        }
        public void Clear()
        {
            _filePath = null;
            _reader = null;
            _context = null;
            if (_stringBuilder != null && _stringBuilder.Capacity > _stringBuilderCapacity * 8)
            {
                _stringBuilder = null;
            }
        }
        private StringBuilder GetStringBuilder()
        {
            return _stringBuilder.Clear();
        }
        private char GetChar(int offset = 0)
        {
            var pos = _index + offset;
            if (pos < _count)
            {
                return _buf[pos];
            }
            if (_isEOF)
            {
                return char.MaxValue;
            }
            var remainCount = _count - _index;
            if (remainCount > 0)
            {
                for (var i = 0; i < remainCount; ++i)
                {
                    _buf[i] = _buf[_index + i];
                }
            }
            var retCount = _reader.Read(_buf, remainCount, _bufLength - remainCount);
            if (retCount == 0)
            {
                _isEOF = true;
            }
            _index = 0;
            _count = remainCount + retCount;
            return GetChar(offset);
        }
        private char GetNextChar()
        {
            return GetChar(1);
        }
        private char GetNextNextChar()
        {
            return GetChar(2);
        }
        private void ConsumeChar(bool checkNewLine = false)
        {
            _lastLine = _line;
            _lastColumn = _column;
            if (_index < _count)
            {
                if (checkNewLine)
                {
                    var ch = _buf[_index++];
                    ++_totalIndex;
                    if (SyntaxFacts.IsNewLine(ch))
                    {
                        if (ch == '\r' && GetChar() == '\n')
                        {
                            ++_index;
                            ++_totalIndex;
                        }
                        ++_line;
                        _column = 1;
                        _atLineHead = true;
                    }
                    else
                    {
                        ++_column;
                    }
                }
                else
                {
                    ++_index;
                    ++_totalIndex;
                    ++_column;
                }
            }
        }
        private int _tokenStartIndex;
        private TextPosition _tokenStartPosition;
        private void MarkTokenStart()
        {
            _tokenStartIndex = _totalIndex;
            _tokenStartPosition = new TextPosition(_line, _column);
        }
        private TextSpan CreateFullTextSpan()
        {
            var startIndex = _tokenStartIndex;
            return new TextSpan(_filePath, startIndex, _totalIndex - startIndex, _tokenStartPosition,
                new TextPosition(_lastLine, _lastColumn));
        }
        private Token CreateToken(TokenKind tokenKind, string value = null, SyntaxKind syntaxKind = SyntaxKind.None)
        {
            _atLineHead = false;
            _gotNonTrivalToken = true;
            return new Token((int)tokenKind, value, CreateFullTextSpan(), syntaxKind);
        }
        private TextSpan CreateSingleTextSpan()
        {
            var pos = new TextPosition(_line, _column);
            return new TextSpan(_filePath, _totalIndex, _index < _count ? 1 : 0, pos, pos);
        }
        private Token CreateTokenAndConsumeChar(char ch)
        {
            _atLineHead = false;
            _gotNonTrivalToken = true;
            var token = new Token(ch, null, CreateSingleTextSpan());
            ConsumeChar();
            return token;
        }
        private void ErrorAndThrow(string errMsg, TextSpan textSpan)
        {
            _context.AddDiag(DiagnosticSeverity.Error, Extensions.ParsingErrorCode, errMsg, textSpan);
            throw ParsingException.Instance;
        }
        private void ErrorAndThrow(string errMsg)
        {
            ErrorAndThrow(errMsg, CreateSingleTextSpan());
        }
        public Token GetToken()
        {
            char ch, nextch, nextnextch;
            StringBuilder sb;
            while (true)
            {
                ch = GetChar();
                switch (ch)
                {
                    case char.MaxValue:
                    case '~':
                    case '{':
                    case '}':
                    case '[':
                    case ']':
                    case '(':
                    case ')':
                    case ',':
                    case ';':
                        return CreateTokenAndConsumeChar(ch);
                    case ' ':
                    case '\t':
                        ConsumeChar();
                        break;
                    case '\r':
                    case '\n':
                        ConsumeChar(true);
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
                        ConsumeChar();
                        return CreateIdToken(GetStringBuilder().Append(ch));
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9':
                        nextch = GetNextChar();
                        MarkTokenStart();
                        ConsumeChar();
                        sb = GetStringBuilder().Append(ch);
                        if (ch == '0' && (nextch == 'x' || nextch == 'X'))
                        {
                            ConsumeChar();
                            sb.Append(nextch);
                            while (true)
                            {
                                ch = GetChar();
                                if (IsHexDigit(ch))
                                {
                                    ConsumeChar();
                                    sb.Append(ch);
                                }
                                else
                                {
                                    break;
                                }
                            }
                            ProcessIntegerSuffix(sb);
                            return CreateToken(TokenKind.Number, sb.ToString());
                        }
                        else
                        {
                            while (true)
                            {
                                ch = GetChar();
                                if (IsDecDigit(ch))
                                {
                                    ConsumeChar();
                                    sb.Append(ch);
                                }
                                else if (ch == '.')
                                {
                                    nextch = GetNextChar();
                                    if (IsDecDigit(nextch))
                                    {
                                        ConsumeChar();
                                        ConsumeChar();
                                        return CreateRealNumberToken(sb.Append(ch).Append(nextch), false);
                                    }
                                    else
                                    {
                                        return CreateToken(TokenKind.Number, sb.ToString());
                                    }
                                }
                                else if (ch == 'E' || ch == 'e')
                                {
                                    sb.Append(ch);
                                    ConsumeChar();
                                    return CreateRealNumberToken(sb.Append(ch).Append(nextch), true);
                                }
                                else if (ProcessIntegerSuffix(sb))
                                {
                                    return CreateToken(TokenKind.Number, sb.ToString());
                                }
                                else if (ProcessRealSuffix(sb))
                                {
                                    return CreateToken(TokenKind.Number, sb.ToString());
                                }
                                else
                                {
                                    return CreateToken(TokenKind.Number, sb.ToString());
                                }
                            }
                        }

                    case '.':
                        nextch = GetNextChar();
                        if (IsDecDigit(nextch))
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateRealNumberToken(GetStringBuilder().Append(ch).Append(nextch), false);
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }
                    case '$':
                        return CreateTokenAndConsumeChar(ch);

                    case '/':
                        nextch = GetNextChar();
                        if (nextch == '/')
                        {
                            ConsumeChar();
                            ConsumeChar();
                            while (true)
                            {
                                ch = GetChar();
                                if (ch == char.MaxValue || SyntaxFacts.IsNewLine(ch))
                                {
                                    break;
                                }
                                else
                                    ConsumeChar();
                            }
                        }
                        else if (nextch == '*')
                        {
                            ConsumeChar();
                            ConsumeChar();
                            while (true)
                            {
                                ch = GetChar();
                                if (ch == '*')
                                {
                                    ConsumeChar();
                                    ch = GetChar();
                                    if (ch == '/')
                                    {
                                        ConsumeChar();
                                        break;
                                    }
                                }
                                else if (ch == char.MaxValue)
                                {
                                    ErrorAndThrow("*/ expected.");
                                }
                                else
                                {
                                    ConsumeChar(true);
                                }
                            }
                            _atLineHead = false;
                        }
                        else if (nextch == '=')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.SlashEquals);
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }
                        break;
                    case '@':
                        nextch = GetNextChar();
                        if (nextch == '"')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            sb = GetStringBuilder();
                            while (true)
                            {
                                ch = GetChar();
                                if (ch == '"')
                                {
                                    ConsumeChar();
                                    ch = GetChar();
                                    if (ch == '"')
                                    {
                                        sb.Append('"');
                                        ConsumeChar();
                                    }
                                    else
                                    {
                                        return CreateToken(TokenKind.String, sb.ToString());
                                    }
                                }
                                else if (ch == char.MaxValue)
                                {
                                    ErrorAndThrow("\" expected.");
                                }
                                else if (ch == '\r' && GetNextChar() == '\n')
                                {
                                    sb.Append("\r\n");
                                    ConsumeChar(true);
                                }
                                else
                                {
                                    sb.Append(ch);
                                    ConsumeChar(true);
                                }
                            }
                        }
                        else if (SyntaxFacts.IsIdentifierStartCharacter(nextch))
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateIdToken(GetStringBuilder().Append(nextch), false);
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }
                    case '"':
                        MarkTokenStart();
                        ConsumeChar();
                        sb = GetStringBuilder();
                        while (true)
                        {
                            ch = GetChar();
                            if (ch == '\\')
                            {
                                ConsumeChar();
                                ProcessCharEscapeSequence(sb);
                            }
                            else if (ch == '"')
                            {
                                ConsumeChar();
                                return CreateToken(TokenKind.String, sb.ToString());
                            }
                            else if (ch == char.MaxValue || SyntaxFacts.IsNewLine(ch))
                            {
                                ErrorAndThrow("\" expected.");
                            }
                            else
                            {
                                sb.Append(ch);
                                ConsumeChar();
                            }
                        }
                    case '\'':
                        MarkTokenStart();
                        ConsumeChar();
                        sb = GetStringBuilder();
                        while (true)
                        {
                            ch = GetChar();
                            if (sb.Length == 0)
                            {
                                if (ch == '\\')
                                {
                                    ConsumeChar();
                                    ProcessCharEscapeSequence(sb);
                                }
                                else if (ch == '\'' || ch == char.MaxValue || SyntaxFacts.IsNewLine(ch))
                                {
                                    ErrorAndThrow("Character expected.");
                                }
                                else
                                {
                                    sb.Append(ch);
                                    ConsumeChar();
                                }
                            }
                            else if (ch == '\'')
                            {
                                ConsumeChar();
                                return CreateToken(TokenKind.Char, sb.ToString());
                            }
                            else
                            {
                                ErrorAndThrow("' expected.");
                            }
                        }
                    case '|':
                        nextch = GetNextChar();
                        if (nextch == '|')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.BarBar);
                        }
                        else if (nextch == '=')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.BarEquals);
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }
                    case '&':
                        nextch = GetNextChar();
                        if (nextch == '&')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.AmpersandAmpersand);
                        }
                        else if (nextch == '=')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.AmpersandEquals);
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }
                    case '-':
                        nextch = GetNextChar();
                        if (nextch == '-')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.MinusMinus);
                        }
                        else if (nextch == '=')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.MinusEquals);
                        }
                        else if (nextch == '>')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.MinusGreaterThan);
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }
                    case '+':
                        nextch = GetNextChar();
                        if (nextch == '+')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.PlusPlus);
                        }
                        else if (nextch == '=')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.PlusEquals);
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }
                    case '!':
                        nextch = GetNextChar();
                        if (nextch == '=')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.ExclamationEquals);
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }
                    case '=':
                        nextch = GetNextChar();
                        if (nextch == '=')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.EqualsEquals);
                        }
                        else if (nextch == '>')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.EqualsGreaterThan);
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }
                    case '<':
                        nextch = GetNextChar();
                        if (nextch == '=')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.LessThanEquals);
                        }
                        else if (nextch == '<')
                        {
                            nextnextch = GetNextNextChar();
                            if (nextnextch == '=')
                            {
                                MarkTokenStart();
                                ConsumeChar();
                                ConsumeChar();
                                ConsumeChar();
                                return CreateToken(TokenKind.LessThanLessThanEquals);
                            }
                            else
                            {
                                MarkTokenStart();
                                ConsumeChar();
                                ConsumeChar();
                                return CreateToken(TokenKind.LessThanLessThan);
                            }
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }
                    case '>':
                        nextch = GetNextChar();
                        if (nextch == '=')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.GreaterThanEquals);
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }
                    case '*':
                        nextch = GetNextChar();
                        if (nextch == '=')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.AsteriskEquals);
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }
                    case '^':
                        nextch = GetNextChar();
                        if (nextch == '=')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.CaretEquals);
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }
                    case '%':
                        nextch = GetNextChar();
                        if (nextch == '=')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.PercentEquals);
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }
                    case '?':
                        nextch = GetNextChar();
                        if (nextch == '?')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.QuestionQuestion);
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }
                    case ':':
                        nextch = GetNextChar();
                        if (nextch == ':')
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            ConsumeChar();
                            return CreateToken(TokenKind.ColonColon);
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }
                    case '#':
                        /*nextch = GetNextChar();
                        if (nextch == '#')
                        {
                            MarkTokenStart();
                            AdvanceChar(false);
                            AdvanceChar(false);
                            return CreateToken(TokenKind.HashHash);
                        }
                        else*/
                        if (_atLineHead)
                        {
                            ConsumeChar();
                            ProcessPpDirective();
                            break;
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }


                    default:
                        if (SyntaxFacts.IsWhitespace(ch))
                        {
                            ConsumeChar();
                        }
                        else if (SyntaxFacts.IsNewLine(ch))
                        {
                            ConsumeChar(true);
                        }
                        else if (SyntaxFacts.IsIdentifierStartCharacter(ch))
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            return CreateIdToken(GetStringBuilder().Append(ch));
                        }
                        else if (IsDecDigit(ch))
                        {
                            MarkTokenStart();
                            ConsumeChar();
                            sb = GetStringBuilder();
                            sb.Append(ch);
                            //todo
                        }
                        else
                        {
                            return CreateTokenAndConsumeChar(ch);
                        }

                        break;
                }
            }
        }

        private Token CreateIdToken(StringBuilder sb, bool isNormal = true)
        {
            while (true)
            {
                var ch = GetChar();
                if (SyntaxFacts.IsIdentifierPartCharacter(ch))
                {
                    sb.Append(ch);
                    ConsumeChar();
                }
                else
                {
                    var text = sb.ToString();
                    TokenKind kind;
                    SyntaxKind syntaxKind;
                    if (isNormal)
                    {
                        syntaxKind = SyntaxFacts.GetKeywordKind(text);
                        if (syntaxKind != SyntaxKind.None)
                        {
                            kind = TokenKind.ReservedKeyword;
                        }
                        else
                        {
                            kind = TokenKind.NormalIdentifier;
                        }
                    }
                    else
                    {
                        kind = TokenKind.VerbatimIdentifier;
                        syntaxKind = SyntaxKind.None;
                    }
                    return CreateToken(kind, text, syntaxKind);
                }
            }
        }
        private void ProcessCharEscapeSequence(StringBuilder sb)
        {
            var ch = GetChar();
            switch (ch)
            {
                case 'u':
                    {
                        ConsumeChar();
                        int value = 0;
                        for (var i = 0; i < 4; ++i)
                        {
                            ch = GetChar();
                            if (IsHexDigit(ch))
                            {
                                value <<= 4;
                                value |= HexValue(ch);
                                ConsumeChar();
                            }
                            else
                            {
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
            ConsumeChar();
        }
        private bool ProcessIntegerSuffix(StringBuilder sb)
        {
            var ch = GetChar();
            switch (ch)
            {
                case 'U':
                case 'u':
                    sb.Append(ch);
                    ConsumeChar();
                    ch = GetChar();
                    if (ch == 'L' || ch == 'l')
                    {
                        sb.Append(ch);
                        ConsumeChar();
                    }
                    return true;
                case 'L':
                case 'l':
                    sb.Append(ch);
                    ConsumeChar();
                    ch = GetChar();
                    if (ch == 'U' || ch == 'u')
                    {
                        sb.Append(ch);
                        ConsumeChar();
                    }
                    return true;
            }
            return false;
        }
        private bool ProcessRealSuffix(StringBuilder sb)
        {
            var ch = GetChar();
            switch (ch)
            {
                case 'F':
                case 'f':
                case 'D':
                case 'd':
                case 'M':
                case 'm':
                    sb.Append(ch);
                    ConsumeChar();
                    return true;
            }
            return false;
        }
        private Token CreateRealNumberToken(StringBuilder sb, bool inExponent)
        {
            char ch;
            if (inExponent)
            {
                goto EXPONENT;
            }
            while (true)
            {
                ch = GetChar();
                if (IsDecDigit(ch))
                {
                    ConsumeChar();
                    sb.Append(ch);
                }
                else if (ch == 'E' || ch == 'e')
                {
                    ConsumeChar();
                    sb.Append(ch);
                    goto EXPONENT;
                }
                else
                {
                    ProcessRealSuffix(sb);
                    return CreateToken(TokenKind.Number, sb.ToString());
                }
            }
        EXPONENT:
            ch = GetChar();
            if (ch == '+' || ch == '-')
            {
                sb.Append(ch);
                ConsumeChar();
                ch = GetChar();
            }
            if (IsDecDigit(ch))
            {
                sb.Append(ch);
                ConsumeChar();
                while (true)
                {
                    ch = GetChar();
                    if (IsDecDigit(ch))
                    {
                        sb.Append(ch);
                        ConsumeChar();
                    }
                    else
                    {
                        ProcessRealSuffix(sb);
                        return CreateToken(TokenKind.Number, sb.ToString());
                    }
                }
            }
            else
            {
                ErrorAndThrow("Decimal digit expected.");
                return default(Token);
            }
        }
        private bool? ProcessPpDirective()
        {
            TextSpan ts;
            var ppDirective = GetPpIdentifier(out ts);
            if (ppDirective == "region")
            {
                ++_ppRegionCount;
                ProcessPpRegionComment();
                return null;
            }
            if (ppDirective == "endregion")
            {
                if (--_ppRegionCount < 0)
                {
                    ErrorAndThrow("Unexpected #endregion.", ts);
                }
                ProcessPpRegionComment();
                return null;
            }
            if (ppDirective == "define")
            {
                if (_gotNonTrivalToken)
                {
                    ErrorAndThrow("Unexpected #define.", ts);
                }
                _ppSymbolSet.Add(ppDirective);
                GetPpExprToken();
                CheckPpNewLine();
                return null;
            }
            if (ppDirective == "undef")
            {
                if (_gotNonTrivalToken)
                {
                    ErrorAndThrow("Unexpected #undef.", ts);
                }
                _ppSymbolSet.Remove(ppDirective);
                GetPpExprToken();
                CheckPpNewLine();
                return null;
            }
            _gotNonTrivalToken = true;
            bool conditionValue;
            var stack = _ppConditionStack;
            if (ppDirective == "if")
            {
                stack.Push(new PpCondition(false, conditionValue = PpExpression()));
            }
            else if (ppDirective == "elif")
            {
                if (stack.Count == 0 || stack.Peek().IsElse)
                {
                    ErrorAndThrow("Unexpected #elif.", ts);
                }
                stack.Pop();
                stack.Push(new PpCondition(false, conditionValue = PpExpression()));
            }
            else if (ppDirective == "else")
            {
                if (stack.Count == 0 || stack.Peek().IsElse)
                {
                    ErrorAndThrow("Unexpected #else.", ts);
                }
                stack.Push(new PpCondition(true, conditionValue = !stack.Pop().Value));
                GetPpExprToken();
            }
            else if (ppDirective == "endif")
            {
                if (stack.Count == 0)
                {
                    ErrorAndThrow("Unexpected #endif.", ts);
                }
                stack.Pop();
                conditionValue = true;
                GetPpExprToken();
            }
            else
            {
                ErrorAndThrow("Invalid preprocessor directive.", ts);
                conditionValue = false;
            }
            CheckPpNewLine();
            if (stack.Count > 0)
            {
                conditionValue = stack.Peek().Value && conditionValue;
            }
            if (conditionValue)
            {
                return true;
            }
            while (true)
            {
                var ch = GetChar();
                if (SyntaxFacts.IsNewLine(ch))
                {
                    ConsumeChar(true);
                }
                else
                {
                    ConsumeChar();
                    if (ch == char.MaxValue)
                    {
                        return true;
                    }
                    else if (ch == '#')
                    {
                        if (_atLineHead)
                        {
                            if (ProcessPpDirective() == true)
                            {
                                return true;
                            }
                        }
                    }
                    else if (!SyntaxFacts.IsWhitespace(ch))
                    {
                        _atLineHead = false;
                    }
                }
            }
        }
        private void ProcessPpRegionComment()
        {
            while (true)
            {
                var ch = GetChar();
                if (ch == char.MaxValue || SyntaxFacts.IsNewLine(ch))
                {
                    return;
                }
                ConsumeChar();
            }
        }
        private void CheckPpNewLine()
        {
            var nextToken = _ppExprToken.Value;
            _ppExprToken = null;
            var nextTokenKind = nextToken.Kind;
            if (nextTokenKind != (int)TokenKind.SingleLineComment && nextTokenKind != char.MaxValue &&
                (nextTokenKind < 0 || !SyntaxFacts.IsNewLine((char)nextTokenKind)))
            {
                ErrorAndThrow("New line expected.", nextToken.TextSpan);
            }
        }
        private struct PpCondition
        {
            internal PpCondition(bool isElse, bool value)
            {
                IsElse = isElse;
                Value = value;
            }
            internal readonly bool IsElse;
            internal readonly bool Value;
        }
        private string TryGetPpIdentifier(out TextSpan ts)
        {
            var ch = GetChar();
            while (SyntaxFacts.IsWhitespace(ch))
            {
                ConsumeChar();
                ch = GetChar();
            }
            if (SyntaxFacts.IsIdentifierStartCharacter(ch))
            {
                var sb = GetStringBuilder();
                sb.Append(ch);
                MarkTokenStart();
                ConsumeChar();
                while (true)
                {
                    ch = GetChar();
                    if (SyntaxFacts.IsIdentifierPartCharacter(ch))
                    {
                        sb.Append(ch);
                        ConsumeChar();
                    }
                    else
                    {
                        break;
                    }
                }
                ts = CreateFullTextSpan();
                return sb.ToString();
            }
            ts = default(TextSpan);
            return null;
        }
        private string GetPpIdentifier(out TextSpan ts)
        {
            var s = TryGetPpIdentifier(out ts);
            if (s == null)
            {
                ErrorAndThrow("Identifier expected.");
            }
            return s;
        }
        private bool PpExpression()
        {
            var result = PpAndExpression();
            while (true)
            {
                if (GetPpExprToken().TokenKind == TokenKind.BarBar)
                {
                    ConsumePpExprToken();
                    result = result || PpAndExpression();
                }
                else
                {
                    break;
                }
            }
            return result;
        }
        private bool PpAndExpression()
        {
            var result = PpEqualityExpression();
            while (true)
            {
                if (GetPpExprToken().TokenKind == TokenKind.AmpersandAmpersand)
                {
                    ConsumePpExprToken();
                    result = result && PpEqualityExpression();
                }
                else
                {
                    break;
                }
            }
            return result;
        }
        private bool PpEqualityExpression()
        {
            var result = PpUnaryExpression();
            while (true)
            {
                var tokenKind = GetPpExprToken().TokenKind;
                if (tokenKind == TokenKind.EqualsEquals)
                {
                    ConsumePpExprToken();
                    result = result == PpUnaryExpression();
                }
                else if (tokenKind == TokenKind.ExclamationEquals)
                {
                    ConsumePpExprToken();
                    result = result != PpUnaryExpression();
                }
                else
                {
                    break;
                }
            }
            return result;
        }
        private bool PpUnaryExpression()
        {
            if (GetPpExprToken().Kind == '!')
            {
                ConsumePpExprToken();
                return !PpUnaryExpression();
            }
            return PpPrimaryExpression();
        }
        private bool PpPrimaryExpression()
        {
            var token = GetPpExprToken();
            var tokenKind = token.Kind;
            if (tokenKind == (int)TokenKind.NormalIdentifier)
            {
                ConsumePpExprToken();
                var text = token.Value;
                if (text == "true")
                {
                    return true;
                }
                if (text == "false")
                {
                    return false;
                }
                return _ppSymbolSet.Contains(text);
            }
            if (tokenKind == '(')
            {
                ConsumePpExprToken();
                var result = PpExpression();
                token = GetPpExprToken();
                if (token.Kind != ')')
                {
                    ErrorAndThrow(") expected.", token.TextSpan);
                }
                ConsumePpExprToken();
                return result;
            }
            ErrorAndThrow("Identifier expected.", token.TextSpan);
            return false;
        }
        private Token GetPpExprToken()
        {
            return (_ppExprToken ?? (_ppExprToken = GetPpExprTokenCore())).Value;
        }
        private void ConsumePpExprToken()
        {
            _ppExprToken = null;
        }
        private Token GetPpExprTokenCore()
        {
            TextSpan ts;
            var s = TryGetPpIdentifier(out ts);
            if (s != null)
            {
                return new Token((int)TokenKind.NormalIdentifier, s, ts);
            }
            var ch = GetChar();
            if (ch == '|')
            {
                var nextch = GetNextChar();
                if (nextch == '|')
                {
                    MarkTokenStart();
                    ConsumeChar();
                    ConsumeChar();
                    return CreateToken(TokenKind.BarBar);
                }
            }
            else if (ch == '&')
            {
                var nextch = GetNextChar();
                if (nextch == '&')
                {
                    MarkTokenStart();
                    ConsumeChar();
                    ConsumeChar();
                    return CreateToken(TokenKind.AmpersandAmpersand);
                }
            }
            else if (ch == '=')
            {
                var nextch = GetNextChar();
                if (nextch == '=')
                {
                    MarkTokenStart();
                    ConsumeChar();
                    ConsumeChar();
                    return CreateToken(TokenKind.EqualsEquals);
                }
            }
            else if (ch == '!')
            {
                var nextch = GetNextChar();
                if (nextch == '=')
                {
                    MarkTokenStart();
                    ConsumeChar();
                    ConsumeChar();
                    return CreateToken(TokenKind.ExclamationEquals);
                }
            }
            else if (ch == '/')
            {
                var nextch = GetNextChar();
                if (nextch == '/')
                {
                    MarkTokenStart();
                    ConsumeChar();
                    ConsumeChar();
                    while (true)
                    {
                        ch = GetChar();
                        if (ch == char.MaxValue || SyntaxFacts.IsNewLine(ch))
                        {
                            break;
                        }
                        ConsumeChar();
                    }
                    return CreateToken(TokenKind.SingleLineComment);
                }
            }
            var tk = new Token(ch, null, CreateSingleTextSpan());
            ConsumeChar(true);
            return tk;
        }


        #region helpers
        private static bool IsDecDigit(char ch)
        {
            return ch >= '0' && ch <= '9';
        }
        private static bool IsHexDigit(char ch)
        {
            return (ch >= '0' && ch <= '9') ||
                   (ch >= 'A' && ch <= 'F') ||
                   (ch >= 'a' && ch <= 'f');
        }
        private static int DecValue(char ch)
        {
            return ch - '0';
        }
        private static int HexValue(char ch)
        {
            return (ch >= '0' && ch <= '9') ? ch - '0' : (ch & 0xdf) - 'A' + 10;
        }

        #endregion helpers
    }


}

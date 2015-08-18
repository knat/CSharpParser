using System;
using System.Collections.Generic;
using System.IO;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace CSharpParser
{
    public delegate bool Creator<T>(out T item);//where T : SyntaxNode;
    public delegate bool IntArgCreator<T>(int arg, out T item);//where T : SyntaxNode;
    public abstract class ParserBase
    {
        protected ParserBase()
        {
            _ExternAliasDirectiveSyntaxCreator = ExternAliasDirective;
            _ExpressionSyntaxCreator = Expression;
            _TypeSyntaxCreator = Type;
            _ArrayRankSpecifierSyntaxCreator = ArrayRankSpecifier;
        }
        protected virtual void Init(string filePath, TextReader reader, ParsingContext context, IEnumerable<string> ppSymbols)
        {
            _lexer = Lexer.Get(filePath, reader, context, ppSymbols);
            _filePath = filePath;
            _context = context;
            _tokenIndex = context.TokenList.Count;
        }
        protected virtual void Clear()
        {
            if (_lexer != null)
            {
                _lexer.Clear();
            }
            _filePath = null;
            _context = null;
        }
        private Lexer _lexer;
        private string _filePath;
        protected ParsingContext _context;
        protected int _tokenIndex;
        //

        protected readonly Creator<ExternAliasDirectiveSyntax> _ExternAliasDirectiveSyntaxCreator;

        protected readonly Creator<ExpressionSyntax> _ExpressionSyntaxCreator;
        protected readonly Creator<TypeSyntax> _TypeSyntaxCreator;
        protected readonly IntArgCreator<ArrayRankSpecifierSyntax> _ArrayRankSpecifierSyntaxCreator;

        protected static readonly Func<OmittedArraySizeExpressionSyntax> _OmittedArraySizeExpressionSyntaxCreator = SyntaxFactory.OmittedArraySizeExpression;
        protected static readonly Func<OmittedTypeArgumentSyntax> _OmittedTypeArgumentSyntaxCreator = SyntaxFactory.OmittedTypeArgument;


        //
        protected void Error(int code, string errMsg, TextSpan textSpan)
        {
            _context.AddDiag(DiagnosticSeverity.Error, code, errMsg, textSpan);
        }
        protected void Throw()
        {
            throw ParsingException.Instance;
        }
        protected void ErrorAndThrow(string errMsg, TextSpan textSpan)
        {
            Error(Extensions.ParsingErrorCode, errMsg, textSpan);
            Throw();
        }
        protected void ErrorAndThrow(string errMsg)
        {
            ErrorAndThrow(errMsg, GetToken().TextSpan);
        }

        protected Token GetToken(int offset = 0)
        {
            var idx = _tokenIndex + offset;
            var tokenList = _context.TokenList;
            while (idx >= tokenList.Count)
            {
                var token = _lexer.GetToken();
                token.Index = tokenList.Count;
                tokenList.Add(token);
            }
            return tokenList[idx];
        }
        //protected void ConsumeToken()
        //{
        //    ++_tokenIndex;
        //}
        protected bool PeekToken(int offset, int kind)
        {
            return GetToken(offset).Kind == kind;
        }
        protected bool PeekToken(int offset, int kind1, int kind2)
        {
            var kind = GetToken(offset).Kind;
            return kind == kind1 || kind == kind2;
        }
        protected bool PeekToken(int offset, int kind1, int kind2, int kind3)
        {
            var kind = GetToken(offset).Kind;
            return kind == kind1 || kind == kind2 || kind == kind3;
        }
        protected bool PeekToken(int offset, int kind1, int kind2, int kind3, int kind4)
        {
            var kind = GetToken(offset).Kind;
            return kind == kind1 || kind == kind2 || kind == kind3 || kind == kind4;
        }

        protected SyntaxToken Identifier(Token token)
        {
            var value = token.Value;
            if (token.IsNormalIdentifier)
            {
                return SyntaxFactory.Identifier(value).Attach(token.Index);
            }
            return SyntaxFactory.Identifier(default(SyntaxTriviaList), SyntaxKind.IdentifierToken,
                   "@" + value, value, default(SyntaxTriviaList)).Attach(token.Index);
        }
        protected bool Identifier(out SyntaxToken result)
        {
            var token = GetToken();
            if (token.IsIdentifier)
            {
                result = Identifier(token);
                return true;
            }
            result = default(SyntaxToken);
            return false;
        }
        protected SyntaxToken IdentifierExpected()
        {
            SyntaxToken result;
            if (!Identifier(out result))
            {
                IdentifierExpectedErrorAndThrow();
            }
            return result;
        }
        protected void IdentifierExpectedErrorAndThrow()
        {
            ErrorAndThrow("Identifier expected.");
        }

        //private static SyntaxKind GetAllKeywordKind(string text)
        //{
        //    var kind = SyntaxFacts.GetKeywordKind(text);
        //    if (kind == SyntaxKind.None)
        //    {
        //        kind = SyntaxFacts.GetContextualKeywordKind(text);
        //    }
        //    return kind;
        //}
        //protected bool Keyword(string text, out SyntaxToken result)
        //{
        //    var token = GetToken();
        //    if (token.IsKeyword(text))
        //    {
        //        ConsumeToken();
        //        result = Token(GetAllKeywordKind(text), token.Index);
        //        return true;
        //    }
        //    result = default(SyntaxToken);
        //    return false;
        //}
        //protected SyntaxToken KeywordExpected(string text)
        //{
        //    SyntaxToken result;
        //    if (!Keyword(text, out result))
        //    {
        //        ErrorAndThrow(text + " expected.");
        //    }
        //    return result;
        //}
        //protected bool Keyword(HashSet<string> keywordSet, out SyntaxToken result)
        //{
        //    var token = GetToken();
        //    if (token.IsNormalIdentifier)
        //    {
        //        var text = token.Value;
        //        if (keywordSet.Contains(text))
        //        {
        //            ConsumeToken();
        //            result = Token(GetAllKeywordKind(text), token.Index);
        //            return true;
        //        }
        //    }
        //    result = default(SyntaxToken);
        //    return false;
        //}
        protected bool ReservedKeyword(SyntaxKind kind, out SyntaxToken result)
        {
            var token = GetToken();
            if (token.IsReservedKeyword && token.SyntaxKind == kind)
            {
                ++_tokenIndex;
                result = Token(kind, token.Index);
                return true;
            }
            result = default(SyntaxToken);
            return false;
        }

        protected static SyntaxToken Token(SyntaxKind kind, int index)
        {
            return SyntaxFactory.Token(kind).Attach(index);
        }
        protected static SyntaxToken Token(int kind, int index)
        {
            return Token(_tokenKindMap[kind], index);
        }

        private static readonly Dictionary<int, SyntaxKind> _tokenKindMap = new Dictionary<int, SyntaxKind> {
            {'~', SyntaxKind.TildeToken },
            {'!', SyntaxKind.ExclamationToken },
            {'$', SyntaxKind.DollarToken },
            {'%', SyntaxKind.PercentToken },
            {'^', SyntaxKind.CaretToken },
            {'&', SyntaxKind.AmpersandToken },
            {'*', SyntaxKind.AsteriskToken },
            {'(', SyntaxKind.OpenParenToken },
            {')', SyntaxKind.CloseParenToken },
            {'-', SyntaxKind.MinusToken },
            {'+', SyntaxKind.PlusToken },
            {'=', SyntaxKind.EqualsToken },
            {'{', SyntaxKind.OpenBraceToken },
            {'}', SyntaxKind.CloseBraceToken },
            {'[', SyntaxKind.OpenBracketToken },
            {']', SyntaxKind.CloseBracketToken },
            {'|', SyntaxKind.BarToken },
            //{'\\', SyntaxKind.BackslashToken },
            {':', SyntaxKind.ColonToken },
            {';', SyntaxKind.SemicolonToken },
            {'"', SyntaxKind.DoubleQuoteToken },
            {'\'', SyntaxKind.SingleQuoteToken },
            {'<', SyntaxKind.LessThanToken },
            {',', SyntaxKind.CommaToken },
            {'>', SyntaxKind.GreaterThanToken },
            {'.', SyntaxKind.DotToken },
            {'?', SyntaxKind.QuestionToken },
            {'#', SyntaxKind.HashToken },
            {'/', SyntaxKind.SlashToken },
            {(int)TokenKind.BarBar, SyntaxKind.BarBarToken },
            {(int)TokenKind.AmpersandAmpersand, SyntaxKind.AmpersandAmpersandToken },
            {(int)TokenKind.MinusMinus, SyntaxKind.MinusMinusToken },
            {(int)TokenKind.PlusPlus, SyntaxKind.PlusPlusToken },
            {(int)TokenKind.ColonColon, SyntaxKind.ColonColonToken },
            {(int)TokenKind.QuestionQuestion, SyntaxKind.QuestionQuestionToken },
            {(int)TokenKind.MinusGreaterThan, SyntaxKind.MinusGreaterThanToken },
            {(int)TokenKind.ExclamationEquals, SyntaxKind.ExclamationEqualsToken },
            {(int)TokenKind.EqualsEquals, SyntaxKind.EqualsEqualsToken },
            {(int)TokenKind.EqualsGreaterThan, SyntaxKind.EqualsGreaterThanToken },
            {(int)TokenKind.LessThanEquals, SyntaxKind.LessThanEqualsToken },
            {(int)TokenKind.LessThanLessThan, SyntaxKind.LessThanLessThanToken },
            {(int)TokenKind.LessThanLessThanEquals, SyntaxKind.LessThanLessThanEqualsToken },
            {(int)TokenKind.GreaterThanEquals, SyntaxKind.GreaterThanEqualsToken },
            //{(int)TokenKind.GreaterThanGreaterThan, SyntaxKind.GreaterThanGreaterThanToken },
            //{(int)TokenKind.GreaterThanGreaterThanEquals, SyntaxKind.GreaterThanGreaterThanEqualsToken },
            {(int)TokenKind.SlashEquals, SyntaxKind.SlashEqualsToken },
            {(int)TokenKind.AsteriskEquals, SyntaxKind.AsteriskEqualsToken },
            {(int)TokenKind.BarEquals, SyntaxKind.BarEqualsToken },
            {(int)TokenKind.AmpersandEquals, SyntaxKind.AmpersandEqualsToken },
            {(int)TokenKind.PlusEquals, SyntaxKind.PlusEqualsToken },
            {(int)TokenKind.MinusEquals, SyntaxKind.MinusEqualsToken },
            {(int)TokenKind.CaretEquals, SyntaxKind.CaretEqualsToken },
            {(int)TokenKind.PercentEquals, SyntaxKind.PercentEqualsToken },

        };
        protected bool Token(int kind, out SyntaxToken result)
        {
            var token = GetToken();
            if (token.Kind == kind)
            {
                ++_tokenIndex;
                result = Token(_tokenKindMap[kind], token.Index);
                return true;
            }
            result = default(SyntaxToken);
            return false;
        }
        protected SyntaxToken TokenExpected(int kind)
        {
            SyntaxToken result;
            if (!Token(kind, out result))
            {
                ErrorAndThrow(SyntaxFacts.GetText(_tokenKindMap[kind]) + " expected.");
            }
            return result;
        }
        //protected bool Token(int kind1, int kind2, SyntaxKind syntaxKind, out SyntaxToken result)
        //{
        //    var token1 = GetToken();
        //    if (token1.Kind == kind1)
        //    {
        //        var token2 = GetToken(1);
        //        if (token2.Kind == kind2 && token1.TextSpan.StartIndex + 1 == token2.TextSpan.StartIndex)
        //        {
        //            ConsumeToken();
        //            ConsumeToken();
        //            result = SyntaxFactory.Token(syntaxKind).Attach(token1.Index, token2.Index);
        //            return true;
        //        }
        //    }
        //    result = default(SyntaxToken);
        //    return false;
        //}

        protected SyntaxList<T> List<T>(Creator<T> itemCreator) where T : SyntaxNode
        {
            T singleItem = null;
            List<T> itemList = null;
            while (true)
            {
                T item;
                if (itemCreator(out item))
                {
                    if (singleItem == null)
                    {
                        singleItem = item;
                    }
                    else
                    {
                        if (itemList == null)
                        {
                            itemList = new List<T>();
                            itemList.Add(singleItem);
                        }
                        itemList.Add(item);
                    }
                }
                else
                {
                    break;
                }
            }
            if (itemList != null)
            {
                return SyntaxFactory.List(itemList);
            }
            if (singleItem != null)
            {
                return SyntaxFactory.SingletonList(singleItem);
            }
            return default(SyntaxList<T>);
        }
        protected SyntaxList<T> List<T>(int arg, IntArgCreator<T> itemCreator) where T : SyntaxNode
        {
            T singleItem = null;
            List<T> itemList = null;
            while (true)
            {
                T item;
                if (itemCreator(arg, out item))
                {
                    if (singleItem == null)
                    {
                        singleItem = item;
                    }
                    else
                    {
                        if (itemList == null)
                        {
                            itemList = new List<T>();
                            itemList.Add(singleItem);
                        }
                        itemList.Add(item);
                    }
                }
                else
                {
                    break;
                }
            }
            if (itemList != null)
            {
                return SyntaxFactory.List(itemList);
            }
            if (singleItem != null)
            {
                return SyntaxFactory.SingletonList(singleItem);
            }
            return default(SyntaxList<T>);
        }
        protected SeparatedSyntaxList<T> SeparatedList<T>(Creator<T> itemCreator, Func<T> omittedItemCreator = null,
            bool allowTrailingSeparator = false, int separatorKind = ',') where T : SyntaxNode
        {
            T singleItem = null;
            List<T> itemList = null;
            SyntaxToken? singleSeparator = null;
            List<SyntaxToken> separatorList = null;
            var useNormal = false;
        START:
            if (omittedItemCreator == null || useNormal)
            {
                while (true)
                {
                    var lastIsSeparator = false;
                    T node;
                    if (itemCreator(out node))
                    {
                        if (singleItem == null)
                        {
                            singleItem = node;
                        }
                        else
                        {
                            if (itemList == null)
                            {
                                itemList = new List<T>();
                                itemList.Add(singleItem);
                            }
                            itemList.Add(node);
                        }
                        SyntaxToken separator;
                        if (Token(separatorKind, out separator))
                        {
                            lastIsSeparator = true;
                            if (singleSeparator == null)
                            {
                                singleSeparator = separator;
                            }
                            else
                            {
                                if (separatorList == null)
                                {
                                    separatorList = new List<SyntaxToken>();
                                    separatorList.Add(singleSeparator.Value);
                                }
                                separatorList.Add(separator);
                            }
                        }
                        else
                        {
                            break;
                        }
                    }
                    else
                    {
                        if (lastIsSeparator && !allowTrailingSeparator)
                        {
                            --_tokenIndex;
                            if (separatorList != null)
                            {
                                separatorList.RemoveAt(separatorList.Count - 1);
                            }
                            else
                            {
                                singleSeparator = null;
                            }
                        }
                        break;
                    }
                }
            }
            else
            {
                if (PeekToken(0, separatorKind))
                {
                    while (true)
                    {
                        SyntaxToken separator;
                        if (Token(separatorKind, out separator))
                        {
                            if (singleSeparator == null)
                            {
                                singleSeparator = separator;
                            }
                            else
                            {
                                if (separatorList == null)
                                {
                                    separatorList = new List<SyntaxToken>();
                                    separatorList.Add(singleSeparator.Value);
                                }
                                separatorList.Add(separator);
                            }
                        }
                        else
                        {
                            break;
                        }
                    }
                    var separatorCount = separatorList != null ? separatorList.Count : (singleSeparator != null ? 1 : 0);
                    if (separatorCount > 0)
                    {
                        itemList = new List<T>(separatorCount + 1);
                        for (var i = 0; i < separatorCount + 1; ++i)
                        {
                            itemList[i] = omittedItemCreator();
                        }
                    }
                }
                else if (itemCreator != null)
                {
                    useNormal = true;
                    goto START;
                }
            }
            if (itemList != null)
            {
                return SyntaxFactory.SeparatedList(itemList, (IEnumerable<SyntaxToken>)separatorList ?? new[] { singleSeparator.Value });
            }
            if (singleItem != null)
            {
                if (singleSeparator != null)
                {
                    return SyntaxFactory.SeparatedList(new[] { singleItem }, new[] { singleSeparator.Value });
                }
                return SyntaxFactory.SingletonSeparatedList(singleItem);
            }
            return default(SeparatedSyntaxList<T>);
        }

        //delegate void D(int a);
        //delegate void D2(ref int a);
        //delegate void D3(out int a);
        //D x = delegate (int s) { };
        //D2 x2 = async delegate (ref int s) { };
        //D3 x3 =  (out int s) => { s = 1; };
        //Action<int, int> g = (a, int b) => { };

        protected bool Block(out BlockSyntax result)
        {
            SyntaxToken st;
            if (Token('{', out st))
            {

            }
            result = null;
            return false;
        }
        //
        protected bool Expression(out ExpressionSyntax result)
        {
            var token = GetToken();
            switch (token.Kind)
            {
                case (int)TokenKind.NormalIdentifier:
                    switch (token.Value)
                    {
                        case "from":
                            {
                                var tkIdx = _tokenIndex;
                                ++_tokenIndex;
                                var tkIdx2 = _tokenIndex;
                                SyntaxToken identifier;
                                SyntaxToken inKeyword;
                                //>from id in
                                if (Identifier(out identifier))
                                {
                                    if (ReservedKeyword(SyntaxKind.InKeyword, out inKeyword))
                                    {
                                        result = QueryExpression(Token(SyntaxKind.FromKeyword, token.Index), null, identifier, inKeyword);
                                        return true;
                                    }
                                }
                                _tokenIndex = tkIdx2;
                                //>from type id in
                                TypeSyntax type;
                                if (Type(out type))
                                {
                                    if (Identifier(out identifier))
                                    {
                                        if (ReservedKeyword(SyntaxKind.InKeyword, out inKeyword))
                                        {
                                            result = QueryExpression(Token(SyntaxKind.FromKeyword, token.Index), type, identifier, inKeyword);
                                            return true;
                                        }
                                    }
                                }
                                _tokenIndex = tkIdx;
                            }
                            break;
                        case "async":
                            {
                                var token2 = GetToken(1);
                                switch (token2.Kind)
                                {
                                    case (int)TokenKind.NormalIdentifier:
                                    case (int)TokenKind.VerbatimIdentifier:
                                        {
                                            var tkIdx = _tokenIndex;
                                            _tokenIndex += 2;
                                            result = SimpleLambdaExpression(token, token2);
                                            if (result != null)
                                            {
                                                return true;
                                            }
                                            _tokenIndex = tkIdx;
                                        }
                                        break;
                                    case '(':
                                        {
                                            var tkIdx = _tokenIndex;
                                            _tokenIndex += 2;
                                            result = ParenthesizedLambdaExpression(token, token2);
                                            if (result != null)
                                            {
                                                return true;
                                            }
                                            _tokenIndex = tkIdx;
                                        }
                                        break;
                                }
                            }
                            break;
                        default:
                            {
                                var tkIdx = _tokenIndex;
                                ++_tokenIndex;
                                result = SimpleLambdaExpression(default(Token), token);
                                if (result != null)
                                {
                                    return true;
                                }
                                _tokenIndex = tkIdx;
                            }
                            break;
                    }
                    break;
                case (int)TokenKind.VerbatimIdentifier:
                    {
                        var tkIdx = _tokenIndex;
                        ++_tokenIndex;
                        result = SimpleLambdaExpression(default(Token), token);
                        if (result != null)
                        {
                            return true;
                        }
                        _tokenIndex = tkIdx;
                    }
                    break;
                case '(':
                    {
                        var tkIdx = _tokenIndex;
                        ++_tokenIndex;
                        result = ParenthesizedLambdaExpression(default(Token), token);
                        if (result != null)
                        {
                            return true;
                        }
                        _tokenIndex = tkIdx;
                    }
                    break;
            }
            if (ConditionalExpression(out result))
            {
                if (!(result is BinaryExpressionSyntax || result is ConditionalExpressionSyntax))
                {
                    SyntaxKind assignKind = SyntaxKind.None;
                    token = GetToken();
                    switch (token.Kind)
                    {
                        case '=':
                            assignKind = SyntaxKind.SimpleAssignmentExpression;
                            break;
                        case (int)TokenKind.PlusEquals:
                            assignKind = SyntaxKind.AddAssignmentExpression;
                            break;
                        case (int)TokenKind.MinusEquals:
                            assignKind = SyntaxKind.SubtractAssignmentExpression;
                            break;
                        case (int)TokenKind.AsteriskEquals:
                            assignKind = SyntaxKind.MultiplyAssignmentExpression;
                            break;
                        case (int)TokenKind.SlashEquals:
                            assignKind = SyntaxKind.DivideAssignmentExpression;
                            break;
                        case (int)TokenKind.PercentEquals:
                            assignKind = SyntaxKind.ModuloAssignmentExpression;
                            break;
                        case (int)TokenKind.AmpersandEquals:
                            assignKind = SyntaxKind.AndAssignmentExpression;
                            break;
                        case (int)TokenKind.BarEquals:
                            assignKind = SyntaxKind.OrAssignmentExpression;
                            break;
                        case (int)TokenKind.CaretEquals:
                            assignKind = SyntaxKind.ExclusiveOrAssignmentExpression;
                            break;
                        case (int)TokenKind.LessThanLessThanEquals:
                            assignKind = SyntaxKind.LeftShiftAssignmentExpression;
                            break;
                        case '>':
                            {
                                var token2 = GetToken(1);
                                if (token2.TokenKind == TokenKind.GreaterThanEquals && token.TextSpan.StartIndex + 1 == token2.TextSpan.StartIndex)
                                {
                                    _tokenIndex += 2;
                                    result = SyntaxFactory.AssignmentExpression(SyntaxKind.RightShiftAssignmentExpression, result,
                                        SyntaxFactory.Token(SyntaxKind.GreaterThanGreaterThanEqualsToken).Attach(token.Index, token2.Index), ExpressionExpected());
                                    return true;
                                }
                            }
                            break;
                    }
                    if (assignKind != SyntaxKind.None)
                    {
                        ++_tokenIndex;
                        result = SyntaxFactory.AssignmentExpression(assignKind, result, Token(token.Kind, token.Index), ExpressionExpected());
                    }
                }
                return true;
            }
            return false;
        }

        private QueryExpressionSyntax QueryExpression(SyntaxToken fromKeyword, TypeSyntax type, SyntaxToken identifier, SyntaxToken inKeyword)
        {

            return null;
        }
        private SimpleLambdaExpressionSyntax SimpleLambdaExpression(Token asyncToken, Token idToken)
        {
            SyntaxToken arrowToken;
            if (Token((int)TokenKind.EqualsGreaterThan, out arrowToken))
            {
                return SyntaxFactory.SimpleLambdaExpression(asyncToken.IsValid ? Token(SyntaxKind.AsyncKeyword, asyncToken.Index) : default(SyntaxToken),
                    SyntaxFactory.Parameter(Identifier(idToken)), arrowToken, LambdaExpressionBody());
            }
            return null;
        }
        private CSharpSyntaxNode LambdaExpressionBody()
        {
            if (PeekToken(0, '{'))
            {
                BlockSyntax b;
                Block(out b);
                return b;
            }
            return ExpressionExpected();
        }
        private ParenthesizedLambdaExpressionSyntax ParenthesizedLambdaExpression(Token asyncToken, Token openParenToken)
        {
            List<ParameterSyntax> paraList = null;
            List<SyntaxToken> commaList = null;
            SyntaxToken closeParen, arrow;
            while (true)
            {
                var token = GetToken();
                switch (token.Kind)
                {
                    case (int)TokenKind.ReservedKeyword:
                        switch (token.SyntaxKind)
                        {
                            case SyntaxKind.RefKeyword:
                            case SyntaxKind.OutKeyword:
                                ++_tokenIndex;
                                Extensions.CreateAndAdd(ref paraList, SyntaxFactory.Parameter(default(SyntaxList<AttributeListSyntax>),
                                     SyntaxFactory.TokenList(Token(token.SyntaxKind, token.Index)), TypeExpected(), IdentifierExpected(), null));
                                break;
                            default:
                                if (SyntaxFacts.IsPredefinedType(token.SyntaxKind))
                                {
                                    ++_tokenIndex;
                                    SyntaxToken id;
                                    if (!Identifier(out id))
                                    {
                                        if (paraList == null)
                                        {
                                            return null;
                                        }
                                        IdentifierExpectedErrorAndThrow();
                                    }
                                    Extensions.CreateAndAdd(ref paraList, SyntaxFactory.Parameter(default(SyntaxList<AttributeListSyntax>),
                                         default(SyntaxTokenList), SyntaxFactory.PredefinedType(Token(token.SyntaxKind, token.Index)),
                                         id, null));
                                }
                                else
                                {
                                    if (paraList == null)
                                    {
                                        return null;
                                    }
                                    IdentifierExpectedErrorAndThrow();
                                }
                                break;
                        }
                        break;
                    case (int)TokenKind.NormalIdentifier:
                    case (int)TokenKind.VerbatimIdentifier:
                        {
                            var token2 = GetToken(1);
                            switch (token2.Kind)
                            {
                                case ',':
                                    _tokenIndex += 2;
                                    Extensions.CreateAndAdd(ref paraList, SyntaxFactory.Parameter(Identifier(token)));
                                    Extensions.CreateAndAdd(ref commaList, Token(SyntaxKind.CommaToken, token2.Index));
                                    break;
                                case ')':
                                    {
                                        _tokenIndex += 2;
                                        var token3 = GetToken();
                                        if (token3.TokenKind == TokenKind.EqualsGreaterThan)
                                        {
                                            ++_tokenIndex;
                                            Extensions.CreateAndAdd(ref paraList, SyntaxFactory.Parameter(Identifier(token)));
                                            closeParen = Token(SyntaxKind.CloseParenToken, token2.Index);
                                            arrow = Token(SyntaxKind.EqualsGreaterThanToken, token3.Index);
                                            goto END;
                                        }
                                        else
                                        {
                                            if (paraList == null)
                                            {
                                                return null;
                                            }
                                            ErrorAndThrow("=> expected.");
                                        }
                                    }
                                    break;
                                default:
                                    {
                                        var type = TypeExpected();
                                        SyntaxToken id;
                                        if (!Identifier(out id))
                                        {
                                            if (paraList == null)
                                            {
                                                return null;
                                            }
                                            IdentifierExpectedErrorAndThrow();
                                        }
                                        Extensions.CreateAndAdd(ref paraList, SyntaxFactory.Parameter(default(SyntaxList<AttributeListSyntax>),
                                             default(SyntaxTokenList), type, id, null));
                                    }
                                    break;
                            }
                        }
                        break;
                    case ',':
                        if (paraList == null || commaList != null && commaList.Count == paraList.Count)
                        {
                            IdentifierExpected();
                        }
                        ++_tokenIndex;
                        Extensions.CreateAndAdd(ref commaList, Token(SyntaxKind.CommaToken, token.Index));
                        break;
                    case ')':
                        if (paraList != null && commaList != null && commaList.Count == paraList.Count)
                        {
                            IdentifierExpected();
                        }
                        ++_tokenIndex;
                        closeParen = Token(SyntaxKind.CloseParenToken, token.Index);
                        arrow = TokenExpected((int)TokenKind.EqualsGreaterThan);
                        goto END;
                }

            }
        END:
            return SyntaxFactory.ParenthesizedLambdaExpression(asyncToken.IsValid ? Token(SyntaxKind.AsyncKeyword, asyncToken.Index) : default(SyntaxToken),
                SyntaxFactory.ParameterList(Token(SyntaxKind.OpenParenToken, openParenToken.Index),
                    paraList == null ? default(SeparatedSyntaxList<ParameterSyntax>) : SyntaxFactory.SeparatedList(paraList, commaList), closeParen),
                arrow, LambdaExpressionBody());
        }

        protected void ExpressionExpectedErrorAndThrow()
        {
            ErrorAndThrow("Expression expected.");
        }
        protected ExpressionSyntax ExpressionExpected()
        {
            ExpressionSyntax result;
            if (!Expression(out result))
            {
                ExpressionExpectedErrorAndThrow();
            }
            return result;
        }
        protected bool ConditionalExpression(out ExpressionSyntax result)
        {
            if (CoalesceExpression(out result))
            {
                SyntaxToken questionToken;
                if (Token('?', out questionToken))
                {
                    result = SyntaxFactory.ConditionalExpression(result, questionToken,
                        ExpressionExpected(), TokenExpected(':'), ExpressionExpected());
                }
                return true;
            }
            return false;
        }
        protected bool CoalesceExpression(out ExpressionSyntax result)
        {
            if (LogicalOrExpression(out result))
            {
                SyntaxToken st;
                if (Token((int)TokenKind.QuestionQuestion, out st))
                {
                    ExpressionSyntax right;
                    if (!CoalesceExpression(out right))
                    {
                        ExpressionExpectedErrorAndThrow();
                    }
                    result = SyntaxFactory.BinaryExpression(SyntaxKind.CoalesceExpression, result, st, right);
                }
                return true;
            }
            return false;
        }
        protected bool LogicalOrExpression(out ExpressionSyntax result)
        {
            if (LogicalAndExpression(out result))
            {
                SyntaxToken st;
                while (Token((int)TokenKind.BarBar, out st))
                {
                    ExpressionSyntax right;
                    if (!LogicalAndExpression(out right))
                    {
                        ExpressionExpectedErrorAndThrow();
                    }
                    result = SyntaxFactory.BinaryExpression(SyntaxKind.LogicalOrExpression, result, st, right);
                }
            }
            return result != null;
        }
        protected bool LogicalAndExpression(out ExpressionSyntax result)
        {
            if (BitwiseOrExpression(out result))
            {
                SyntaxToken st;
                while (Token((int)TokenKind.AmpersandAmpersand, out st))
                {
                    ExpressionSyntax right;
                    if (!BitwiseOrExpression(out right))
                    {
                        ExpressionExpectedErrorAndThrow();
                    }
                    result = SyntaxFactory.BinaryExpression(SyntaxKind.LogicalAndExpression, result, st, right);
                }
            }
            return result != null;
        }
        protected bool BitwiseOrExpression(out ExpressionSyntax result)
        {
            if (ExclusiveOrExpression(out result))
            {
                SyntaxToken st;
                while (Token('|', out st))
                {
                    ExpressionSyntax right;
                    if (!ExclusiveOrExpression(out right))
                    {
                        ExpressionExpectedErrorAndThrow();
                    }
                    result = SyntaxFactory.BinaryExpression(SyntaxKind.BitwiseOrExpression, result, st, right);
                }
            }
            return result != null;
        }
        protected bool ExclusiveOrExpression(out ExpressionSyntax result)
        {
            if (BitwiseAndExpression(out result))
            {
                SyntaxToken st;
                while (Token('^', out st))
                {
                    ExpressionSyntax right;
                    if (!BitwiseAndExpression(out right))
                    {
                        ExpressionExpectedErrorAndThrow();
                    }
                    result = SyntaxFactory.BinaryExpression(SyntaxKind.ExclusiveOrExpression, result, st, right);
                }
            }
            return result != null;
        }
        protected bool BitwiseAndExpression(out ExpressionSyntax result)
        {
            if (EqualityExpression(out result))
            {
                SyntaxToken st;
                while (Token('&', out st))
                {
                    ExpressionSyntax right;
                    if (!EqualityExpression(out right))
                    {
                        ExpressionExpectedErrorAndThrow();
                    }
                    result = SyntaxFactory.BinaryExpression(SyntaxKind.BitwiseAndExpression, result, st, right);
                }
            }
            return result != null;
        }
        protected bool EqualityExpression(out ExpressionSyntax result)
        {
            if (RelationalExpression(out result))
            {
                while (true)
                {
                    SyntaxKind exprKind;
                    var token = GetToken();
                    var tokenKind = token.Kind;
                    switch (tokenKind)
                    {
                        case (int)TokenKind.EqualsEquals:
                            exprKind = SyntaxKind.EqualsExpression;
                            break;
                        case (int)TokenKind.ExclamationEquals:
                            exprKind = SyntaxKind.NotEqualsExpression;
                            break;
                        default:
                            return true;
                    }
                    ++_tokenIndex;
                    ExpressionSyntax right;
                    if (!RelationalExpression(out right))
                    {
                        ExpressionExpectedErrorAndThrow();
                    }
                    result = SyntaxFactory.BinaryExpression(exprKind, result, Token(tokenKind, token.Index), right);
                }
            }
            return false;
        }
        protected bool RelationalExpression(out ExpressionSyntax result)
        {
            if (ShiftExpression(out result))
            {
                while (true)
                {
                    SyntaxKind exprKind;
                    var token = GetToken();
                    var tokenKind = token.Kind;
                    switch (tokenKind)
                    {
                        case '<':
                            exprKind = SyntaxKind.LessThanExpression;
                            break;
                        case (int)TokenKind.LessThanEquals:
                            exprKind = SyntaxKind.LessThanOrEqualExpression;
                            break;
                        case '>':
                            {
                                var token2 = GetToken(1);
                                if (token2.TokenKind == TokenKind.GreaterThanEquals && token.TextSpan.StartIndex + 1 == token2.TextSpan.StartIndex)
                                {
                                    return true;
                                }
                                else
                                {
                                    exprKind = SyntaxKind.GreaterThanExpression;
                                }
                            }
                            break;
                        case (int)TokenKind.GreaterThanEquals:
                            exprKind = SyntaxKind.GreaterThanOrEqualExpression;
                            break;
                        case (int)TokenKind.ReservedKeyword:
                            switch (token.SyntaxKind)
                            {
                                case SyntaxKind.IsKeyword:
                                    {
                                        ++_tokenIndex;
                                        TypeSyntax type;
                                        //e.g. var x = o is int ? 1 : 2;
                                        //e.g. var x = o is int? ? 1 : 2;
                                        if (!TypeCore(true, out type))
                                        {
                                            TypeExpectedErrorAndThrow();
                                        }
                                        result = SyntaxFactory.BinaryExpression(SyntaxKind.IsExpression, result, Token(SyntaxKind.IsKeyword, token.Index),
                                            type);
                                    }
                                    continue;
                                case SyntaxKind.AsKeyword:
                                    ++_tokenIndex;
                                    result = SyntaxFactory.BinaryExpression(SyntaxKind.AsExpression, result, Token(SyntaxKind.AsKeyword, token.Index),
                                        TypeExpected());
                                    continue;
                                default:
                                    return true;
                            }
                        default:
                            return true;
                    }
                    ++_tokenIndex;
                    ExpressionSyntax right;
                    if (!ShiftExpression(out right))
                    {
                        ExpressionExpectedErrorAndThrow();
                    }
                    result = SyntaxFactory.BinaryExpression(exprKind, result, Token(tokenKind, token.Index), right);
                }
            }
            return false;
        }
        protected bool ShiftExpression(out ExpressionSyntax result)
        {
            if (AdditiveExpression(out result))
            {
                while (true)
                {
                    SyntaxKind exprKind;
                    SyntaxToken st;
                    var token = GetToken();
                    switch (token.Kind)
                    {
                        case (int)TokenKind.LessThanLessThan:
                            ++_tokenIndex;
                            st = Token(SyntaxKind.LessThanLessThanToken, token.Index);
                            exprKind = SyntaxKind.LeftShiftExpression;
                            break;
                        case '>':
                            {
                                var token2 = GetToken(1);
                                if (token2.Kind == '>' && token.TextSpan.StartIndex + 1 == token2.TextSpan.StartIndex)
                                {
                                    _tokenIndex += 2;
                                    st = SyntaxFactory.Token(SyntaxKind.GreaterThanGreaterThanToken).Attach(token.Index, token2.Index);
                                    exprKind = SyntaxKind.RightShiftExpression;
                                }
                                else
                                {
                                    return true;
                                }
                            }
                            break;
                        default:
                            return true;
                    }
                    ExpressionSyntax right;
                    if (!AdditiveExpression(out right))
                    {
                        ExpressionExpectedErrorAndThrow();
                    }
                    result = SyntaxFactory.BinaryExpression(exprKind, result, st, right);
                }
            }
            return false;
        }
        protected bool AdditiveExpression(out ExpressionSyntax result)
        {
            if (MultiplicativeExpression(out result))
            {
                while (true)
                {
                    SyntaxKind exprKind;
                    var token = GetToken();
                    var tokenKind = token.Kind;
                    switch (tokenKind)
                    {
                        case '+':
                            exprKind = SyntaxKind.AddExpression;
                            break;
                        case '-':
                            exprKind = SyntaxKind.SubtractExpression;
                            break;
                        default:
                            return true;
                    }
                    ++_tokenIndex;
                    ExpressionSyntax right;
                    if (!MultiplicativeExpression(out right))
                    {
                        ExpressionExpectedErrorAndThrow();
                    }
                    result = SyntaxFactory.BinaryExpression(exprKind, result, Token(tokenKind, token.Index), right);
                }
            }
            return false;
        }
        protected bool MultiplicativeExpression(out ExpressionSyntax result)
        {
            if (PrefixUnaryExpression(out result))
            {
                while (true)
                {
                    SyntaxKind exprKind;
                    var token = GetToken();
                    var tokenKind = token.Kind;
                    switch (tokenKind)
                    {
                        case '*':
                            exprKind = SyntaxKind.MultiplyExpression;
                            break;
                        case '/':
                            exprKind = SyntaxKind.DivideExpression;
                            break;
                        case '%':
                            exprKind = SyntaxKind.ModuloExpression;
                            break;
                        default:
                            return true;
                    }
                    ++_tokenIndex;
                    ExpressionSyntax right;
                    if (!PrefixUnaryExpression(out right))
                    {
                        ExpressionExpectedErrorAndThrow();
                    }
                    result = SyntaxFactory.BinaryExpression(exprKind, result, Token(tokenKind, token.Index), right);
                }
            }
            return false;
        }
        protected bool PrefixUnaryExpression(out ExpressionSyntax result)
        {
            var exprKind = SyntaxKind.None;
            var token = GetToken();
            var tokenKind = token.Kind;
            switch (tokenKind)
            {
                case '+':
                    exprKind = SyntaxKind.UnaryPlusExpression;
                    break;
                case '-':
                    exprKind = SyntaxKind.UnaryMinusExpression;
                    break;
                case '!':
                    exprKind = SyntaxKind.LogicalNotExpression;
                    break;
                case '~':
                    exprKind = SyntaxKind.BitwiseNotExpression;
                    break;
                case (int)TokenKind.PlusPlus:
                    exprKind = SyntaxKind.PreIncrementExpression;
                    break;
                case (int)TokenKind.MinusMinus:
                    exprKind = SyntaxKind.PreDecrementExpression;
                    break;
                case '(':
                    {
                        var tkIdx = _tokenIndex;
                        ++_tokenIndex;
                        TypeSyntax type;
                        if (Type(out type))
                        {
                            SyntaxToken closeParen;
                            if (Token(')', out closeParen))
                            {
                                //C# spec 7.7.6
                                bool ok;
                                if (type is PredefinedTypeSyntax)
                                {
                                    ok = true;
                                }
                                else
                                {
                                    var token2 = GetToken();
                                    switch (token2.Kind)
                                    {
                                        case (int)TokenKind.ReservedKeyword:
                                            switch (token2.SyntaxKind)
                                            {
                                                case SyntaxKind.IsKeyword:
                                                case SyntaxKind.AsKeyword:
                                                    ok = false;
                                                    break;
                                                default:
                                                    ok = true;
                                                    break;
                                            }
                                            break;
                                        case (int)TokenKind.NormalIdentifier:
                                        case (int)TokenKind.VerbatimIdentifier:
                                        case (int)TokenKind.String:
                                        case (int)TokenKind.Char:
                                        case (int)TokenKind.Number:
                                        case '~':
                                        case '!':
                                        case '(':
                                            //C# spec's bug?
                                            //case (int)TokenKind.PlusPlus:
                                            //case (int)TokenKind.MinusMinus:
                                            ok = true;
                                            break;
                                        default:
                                            ok = false;
                                            break;
                                    }
                                }
                                if (ok)
                                {
                                    ExpressionSyntax expr;
                                    if (PrefixUnaryExpression(out expr))
                                    {
                                        result = SyntaxFactory.CastExpression(Token(SyntaxKind.OpenParenToken, token.Index), type, closeParen, expr);
                                        return true;
                                    }
                                }
                            }
                        }
                        _tokenIndex = tkIdx;
                    }
                    break;
                case (int)TokenKind.NormalIdentifier:
                    if (token.Value == "await")
                    {
                        var tkIdx = _tokenIndex;
                        ++_tokenIndex;
                        //todo: C# spec ???
                        //e.g. var await = 0; var x = await + 3;
                        bool ok;
                        switch (GetToken().Kind)
                        {
                            case '+':
                            case '-':
                                ok = false;
                                break;
                            default:
                                ok = true;
                                break;
                        }
                        if (ok)
                        {
                            ExpressionSyntax expr;
                            if (PrefixUnaryExpression(out expr))
                            {
                                result = SyntaxFactory.AwaitExpression(Token(SyntaxKind.AwaitKeyword, token.Index), expr);
                                return true;
                            }
                        }
                        _tokenIndex = tkIdx;
                    }
                    break;
            }
            if (exprKind != SyntaxKind.None)
            {
                ++_tokenIndex;
                ExpressionSyntax operand;
                if (!PrefixUnaryExpression(out operand))
                {
                    ExpressionExpectedErrorAndThrow();
                }
                result = SyntaxFactory.PrefixUnaryExpression(exprKind, Token(tokenKind, token.Index), operand);
                return true;
            }
            return PrimaryExpression(out result);
        }
        protected bool PrimaryExpression(out ExpressionSyntax result)
        {
            result = null;
            var token = GetToken();
            var tokenValue = token.Value;
            switch (token.Kind)
            {
                case (int)TokenKind.String:
                    ++_tokenIndex;
                    result = SyntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression,
                        SyntaxFactory.Literal(tokenValue).Attach(token.Index));
                    break;
                case (int)TokenKind.Char:
                    ++_tokenIndex;
                    result = SyntaxFactory.LiteralExpression(SyntaxKind.CharacterLiteralExpression,
                        SyntaxFactory.Literal(tokenValue[0]).Attach(token.Index));
                    break;
                case (int)TokenKind.Number:
                    ++_tokenIndex;
                    result = SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                        SyntaxFactory.ParseToken(tokenValue).Attach(token.Index));
                    break;
                case '(':
                    ++_tokenIndex;
                    result = SyntaxFactory.ParenthesizedExpression(Token(SyntaxKind.OpenParenToken, token.Index), ExpressionExpected(),
                        TokenExpected(')'));
                    break;
                case (int)TokenKind.ReservedKeyword:
                    switch (token.SyntaxKind)
                    {
                        case SyntaxKind.TrueKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.LiteralExpression(SyntaxKind.TrueLiteralExpression,
                                Token(SyntaxKind.TrueKeyword, token.Index));
                            break;
                        case SyntaxKind.FalseKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.LiteralExpression(SyntaxKind.FalseLiteralExpression,
                                Token(SyntaxKind.FalseKeyword, token.Index));
                            break;
                        case SyntaxKind.NullKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression,
                                Token(SyntaxKind.NullKeyword, token.Index));
                            break;
                        case SyntaxKind.ThisKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.ThisExpression(Token(SyntaxKind.ThisKeyword, token.Index));
                            break;
                        case SyntaxKind.BaseKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.BaseExpression(Token(SyntaxKind.BaseKeyword, token.Index));
                            break;
                        case SyntaxKind.TypeOfKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.TypeOfExpression(Token(SyntaxKind.TypeOfKeyword, token.Index),
                                TokenExpected('('), TypeExpected(), TokenExpected(')'));
                            break;
                        case SyntaxKind.DefaultKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.DefaultExpression(Token(SyntaxKind.DefaultKeyword, token.Index),
                                TokenExpected('('), TypeExpected(), TokenExpected(')'));
                            break;
                        case SyntaxKind.CheckedKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.CheckedExpression(SyntaxKind.CheckedExpression, Token(SyntaxKind.CheckedKeyword, token.Index),
                                TokenExpected('('), ExpressionExpected(), TokenExpected(')'));
                            break;
                        case SyntaxKind.UncheckedKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.CheckedExpression(SyntaxKind.UncheckedExpression, Token(SyntaxKind.UncheckedKeyword, token.Index),
                                TokenExpected('('), ExpressionExpected(), TokenExpected(')'));
                            break;
                        //case "async":
                        //    {
                        //        var token2 = GetToken(1);
                        //        //if (token2.IsKeyword("delegate"))
                        //        //{
                        //        //    ConsumeToken();
                        //        //    result = AnonymousMethodExpression(Token(SyntaxKind.AsyncKeyword, token.Index));
                        //        //}
                        //    }
                        //    break;
                        case SyntaxKind.DelegateKeyword:
                            ++_tokenIndex;
                            //result = AnonymousMethodExpression(default(SyntaxToken));
                            break;
                        case SyntaxKind.NewKeyword:
                            ++_tokenIndex;


                            break;
                    }
                    break;
            }
            if (result == null)
            {
                PredefinedTypeSyntax pt;
                if (PredefinedType(out pt))
                {
                    result = pt;
                }
                else
                {
                    NameSyntax n;
                    if (NameStart(true, out n))
                    {
                        result = n;
                    }
                }
            }
            if (result != null)
            {
                while (true)
                {
                    token = GetToken();
                    switch (token.Kind)
                    {
                        case '.':
                            ++_tokenIndex;
                            result = SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, result,
                                Token(SyntaxKind.DotToken, token.Index), SimpleNameExpected(true));
                            break;
                        case (int)TokenKind.PlusEquals:
                            ++_tokenIndex;
                            result = SyntaxFactory.PostfixUnaryExpression(SyntaxKind.PostIncrementExpression, result,
                                Token(SyntaxKind.PlusPlusToken, token.Index));
                            break;
                        case (int)TokenKind.MinusMinus:
                            ++_tokenIndex;
                            result = SyntaxFactory.PostfixUnaryExpression(SyntaxKind.PostDecrementExpression, result,
                                Token(SyntaxKind.MinusMinusToken, token.Index));
                            break;
                        case '(':

                            break;
                        case '[':

                            break;
                        default:
                            return true;
                    }
                }
            }
            return false;
        }
        //private AnonymousMethodExpressionSyntax AnonymousMethodExpression(SyntaxToken asyncKeyword)
        //{
        //    return SyntaxFactory.AnonymousMethodExpression(asyncKeyword, KeywordExpected("delegate"), null, null);
        //}

        protected bool PredefinedType(out PredefinedTypeSyntax result)
        {
            var token = GetToken();
            if (SyntaxFacts.IsPredefinedType(token.SyntaxKind))
            {
                result = SyntaxFactory.PredefinedType(Token(token.SyntaxKind, token.Index));
                return true;
            }
            result = null;
            return false;
        }
        protected bool Type(out TypeSyntax result)
        {
            return TypeCore(false, out result);
        }
        protected bool TypeCore(bool checkNullable, out TypeSyntax result)
        {
            PredefinedTypeSyntax pt;
            if (PredefinedType(out pt))
            {
                result = pt;
            }
            else
            {
                NameSyntax ns;
                if (Name(out ns))
                {
                    result = ns;
                }
                else
                {
                    result = null;
                    return false;
                }
            }
            var token = GetToken();
            if (token.Kind == '?')
            {
                var ok = true;
                if (checkNullable)
                {
                    //C# spec ???
                    switch (GetToken(1).Kind)
                    {
                        case ';':
                        case ',':
                        case ')':
                        case ']':
                        case '}':
                        case '?':
                        case '&':
                        case '|':
                        case '^':
                        case (int)TokenKind.AmpersandAmpersand:
                        case (int)TokenKind.BarBar:
                        case (int)TokenKind.EqualsEquals:
                        case (int)TokenKind.ExclamationEquals:

                            break;
                        default:
                            ok = false;
                            break;
                    }
                }
                if (ok)
                {
                    ++_tokenIndex;
                    result = SyntaxFactory.NullableType(result, Token(SyntaxKind.QuestionToken, token.Index));
                }
            }
            if (PeekToken(0, '['))
            {
                result = SyntaxFactory.ArrayType(result, List((int)ArrayRankSpecifierFlag.None, _ArrayRankSpecifierSyntaxCreator));
            }
            return true;
        }
        protected TypeSyntax TypeExpected()
        {
            TypeSyntax result;
            if (!Type(out result))
            {
                TypeExpectedErrorAndThrow();
            }
            return result;
        }
        protected void TypeExpectedErrorAndThrow()
        {
            ErrorAndThrow("Type expected.");
        }

        protected enum ArrayRankSpecifierFlag
        {
            None = 0,
            AllowExpr = 1,
        }
        protected bool ArrayRankSpecifier(int arg, out ArrayRankSpecifierSyntax result)
        {
            SyntaxToken st;
            if (Token('[', out st))
            {
                result = SyntaxFactory.ArrayRankSpecifier(st,
                    SeparatedList(((ArrayRankSpecifierFlag)arg) == ArrayRankSpecifierFlag.AllowExpr ? _ExpressionSyntaxCreator : null,
                    _OmittedArraySizeExpressionSyntaxCreator), TokenExpected(']'));
                return true;
            }
            result = null;
            return false;
        }
        protected bool Name(out NameSyntax result)
        {
            if (!NameStart(false, out result))
            {
                return false;
            }
            SyntaxToken dot;
            while (Token('.', out dot))
            {
                result = SyntaxFactory.QualifiedName(result, dot, SimpleNameExpected(false));
            }
            return true;
        }
        protected bool NameStart(bool checkTypeArgs, out NameSyntax result)
        {
            var idToken = GetToken();
            if (idToken.IsIdentifier)
            {
                var colonColonToken = GetToken(1);
                if (colonColonToken.TokenKind == TokenKind.ColonColon)
                {
                    IdentifierNameSyntax alias;
                    if (idToken.IsKeyword("global"))
                    {
                        alias = SyntaxFactory.IdentifierName(Token(SyntaxKind.GlobalKeyword, idToken.Index));
                    }
                    else
                    {
                        alias = SyntaxFactory.IdentifierName(Identifier(idToken));
                    }
                    _tokenIndex += 2;
                    result = SyntaxFactory.AliasQualifiedName(alias, Token(SyntaxKind.ColonColonToken, colonColonToken.Index),
                        SimpleNameExpected(checkTypeArgs));
                    return true;
                }
                SimpleNameSyntax sn;
                SimpleName(checkTypeArgs, out sn);
                result = sn;
                return true;
            }
            result = null;
            return false;
        }

        protected bool SimpleName(bool checkTypeArgs, out SimpleNameSyntax result)
        {
            var idToken = GetToken();
            if (idToken.IsIdentifier)
            {
                ++_tokenIndex;
                var ltToken = GetToken();
                if (ltToken.Kind == '<')
                {
                    var tkIdx = _tokenIndex;
                    ++_tokenIndex;
                    var typeArgs = SeparatedList(_TypeSyntaxCreator, _OmittedTypeArgumentSyntaxCreator);
                    SyntaxToken gtToken;
                    if (Token('>', out gtToken))
                    {
                        var ok = true;
                        if (checkTypeArgs)
                        {
                            switch (GetToken().Kind)
                            {
                                //C# spec 7.6.4.2
                                //(  )  ]  }  :  ;  ,  .  ?  ==  !=  |  ^
                                case '(':
                                case ')':
                                case ']':
                                case '}':
                                case ':':
                                case ';':
                                case ',':
                                case '.':
                                case '?':
                                case '|':
                                case '^':
                                case (int)TokenKind.EqualsEquals:
                                case (int)TokenKind.ExclamationEquals:
                                    break;
                                default:
                                    ok = false;
                                    break;
                            }
                        }
                        if (ok)
                        {
                            result = SyntaxFactory.GenericName(Identifier(idToken),
                                SyntaxFactory.TypeArgumentList(Token(SyntaxKind.LessThanToken, ltToken.Index), typeArgs, gtToken));
                            return true;
                        }
                    }
                    _tokenIndex = tkIdx;
                }
                result = SyntaxFactory.IdentifierName(Identifier(idToken));
                return true;
            }
            result = null;
            return false;
        }
        protected SimpleNameSyntax SimpleNameExpected(bool checkTypeArgs)
        {
            SimpleNameSyntax result;
            if (!SimpleName(checkTypeArgs, out result))
            {
                ErrorAndThrow("Simple name expected.");
            }
            return result;
        }
        protected bool IdentifierName(out IdentifierNameSyntax result)
        {
            SyntaxToken st;
            if (Identifier(out st))
            {
                result = SyntaxFactory.IdentifierName(st);
                return true;
            }
            result = null;
            return false;
        }

        //
        //
        //

        protected bool ExternAliasDirective(out ExternAliasDirectiveSyntax result)
        {
            //SyntaxToken externKeyword;
            //if (Keyword("extern", out externKeyword))
            //{
            //    var aliasKeyword = KeywordExpected("alias");
            //    var identifier = IdentifierExpected();
            //    var semicolonToken = TokenExpected(';');
            //    result = SyntaxFactory.ExternAliasDirective(externKeyword, aliasKeyword, identifier, semicolonToken);
            //    return true;
            //}
            result = null;
            return false;
        }




    }

}

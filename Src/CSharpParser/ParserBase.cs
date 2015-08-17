using System;
using System.Collections.Generic;
using System.IO;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;

namespace CSharpParser
{
    public delegate bool Creator<T>(out T item);//where T : SyntaxNode;
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
            //
            _allowArrayRankSpecifierExprs = false;
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
        protected readonly Creator<ArrayRankSpecifierSyntax> _ArrayRankSpecifierSyntaxCreator;

        protected static readonly Func<OmittedArraySizeExpressionSyntax> _OmittedArraySizeExpressionSyntaxCreator = SyntaxFactory.OmittedArraySizeExpression;
        protected static readonly Func<OmittedTypeArgumentSyntax> _OmittedTypeArgumentSyntaxCreator = SyntaxFactory.OmittedTypeArgument;


        //
        protected void Error(int code, string errMsg, TextSpan textSpan)
        {
            _context.AddDiag(DiagnosticSeverity.Error, code, errMsg, textSpan);
        }
        protected void Error(DiagMsg diagMsg, TextSpan textSpan)
        {
            Error((int)diagMsg.Code, diagMsg.GetMessage(), textSpan);
        }
        protected void Throw()
        {
            throw ParsingException.Instance;
        }
        protected void ErrorAndThrow(string errMsg, TextSpan textSpan)
        {
            Error((int)DiagnosticCode.Parsing, errMsg, textSpan);
            Throw();
        }
        protected void ErrorAndThrow(string errMsg)
        {
            ErrorAndThrow(errMsg, GetToken().TextSpan);
        }
        protected void ErrorAndThrow(DiagMsg diagMsg, TextSpan textSpan)
        {
            Error(diagMsg, textSpan);
            Throw();
        }
        protected void ErrorAndThrow(DiagMsg diagMsg)
        {
            ErrorAndThrow(diagMsg, GetToken().TextSpan);
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
        protected void ConsumeToken()
        {
            ++_tokenIndex;
        }
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
            var text = token.Value;
            if (token.IsNormalIdentifier)
            {
                if (SyntaxFacts.GetKeywordKind(text) != SyntaxKind.None)
                {
                    ErrorAndThrow("@ required if identifier is reserved keyword.", token.TextSpan);
                }
                return SyntaxFactory.Identifier(text).Attach(token.Index);
            }
            return SyntaxFactory.Identifier(default(SyntaxTriviaList), SyntaxKind.IdentifierToken,
                   "@" + text, text, default(SyntaxTriviaList)).Attach(token.Index);
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
                ErrorAndThrow("Identifier expected.");
            }
            return result;
        }

        private static SyntaxKind GetAllKeywordKind(string text)
        {
            var kind = SyntaxFacts.GetKeywordKind(text);
            if (kind == SyntaxKind.None)
            {
                kind = SyntaxFacts.GetContextualKeywordKind(text);
            }
            return kind;
        }
        protected bool Keyword(string text, out SyntaxToken result)
        {
            var token = GetToken();
            if (token.IsKeyword(text))
            {
                ConsumeToken();
                result = Token(GetAllKeywordKind(text), token.Index);
                return true;
            }
            result = default(SyntaxToken);
            return false;
        }
        protected SyntaxToken KeywordExpected(string text)
        {
            SyntaxToken result;
            if (!Keyword(text, out result))
            {
                ErrorAndThrow(text + " expected.");
            }
            return result;
        }
        protected bool Keyword(HashSet<string> keywordSet, out SyntaxToken result)
        {
            var token = GetToken();
            if (token.IsNormalIdentifier)
            {
                var text = token.Value;
                if (keywordSet.Contains(text))
                {
                    ConsumeToken();
                    result = Token(GetAllKeywordKind(text), token.Index);
                    return true;
                }
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
                ConsumeToken();
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
        protected bool Token(int kind1, int kind2, SyntaxKind syntaxKind, out SyntaxToken result)
        {
            var token1 = GetToken();
            if (token1.Kind == kind1)
            {
                var token2 = GetToken(1);
                if (token2.Kind == kind2 && token1.TextSpan.StartIndex + 1 == token2.TextSpan.StartIndex)
                {
                    ConsumeToken();
                    ConsumeToken();
                    result = SyntaxFactory.Token(syntaxKind).Attach(token1.Index, token2.Index);
                    return true;
                }
            }
            result = default(SyntaxToken);
            return false;
        }

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
        protected SeparatedSyntaxList<T> SeparatedList<T>(Creator<T> itemCreator, int separatorKind = ',',
            bool allowTrailingSeparator = false, Func<T> omittedItemCreator = null) where T : SyntaxNode
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

        //
        protected bool Expression(out ExpressionSyntax result)
        {

            result = null;
            return false;
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
        protected bool AssignmentExpression(out AssignmentExpressionSyntax result)
        {


            result = null;
            return false;
        }
        private bool _inConditionalExprCondition;
        protected bool ConditionalExpression(out ExpressionSyntax result)
        {
            ExpressionSyntax condition;
            _inConditionalExprCondition = true;
            var b = CoalesceExpression(out condition);
            _inConditionalExprCondition = false;
            if (b)
            {
                SyntaxToken question, colon = default(SyntaxToken);
                ExpressionSyntax whenTrue = null, whenFalse = null;
                if (Token('?', out question))
                {
                    whenTrue = ExpressionExpected();
                    colon = TokenExpected(':');
                    whenFalse = ExpressionExpected();
                }
                if (whenTrue == null)
                {
                    result = condition;
                }
                else
                {
                    result = SyntaxFactory.ConditionalExpression(condition, question, whenTrue, colon, whenFalse);
                }
                return true;
            }
            result = null;
            return false;
        }
        protected bool CoalesceExpression(out ExpressionSyntax result)
        {
            ExpressionSyntax left;
            if (LogicalOrExpression(out left))
            {
                SyntaxToken st;
                ExpressionSyntax right = null;
                if (Token((int)TokenKind.QuestionQuestion, out st))
                {
                    if (!CoalesceExpression(out right))
                    {
                        ExpressionExpectedErrorAndThrow();
                    }
                }
                if (right == null)
                {
                    result = left;
                }
                else
                {
                    result = SyntaxFactory.BinaryExpression(SyntaxKind.CoalesceExpression, left, st, right);
                }
                return true;
            }
            result = null;
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
                    SyntaxKind kind;
                    SyntaxToken st;
                    if (Token((int)TokenKind.EqualsEquals, out st))
                    {
                        kind = SyntaxKind.EqualsExpression;
                    }
                    else if (Token((int)TokenKind.ExclamationEquals, out st))
                    {
                        kind = SyntaxKind.NotEqualsExpression;
                    }
                    else
                    {
                        break;
                    }
                    ExpressionSyntax right;
                    if (!RelationalExpression(out right))
                    {
                        ExpressionExpectedErrorAndThrow();
                    }
                    result = SyntaxFactory.BinaryExpression(kind, result, st, right);
                }
            }
            return result != null;
        }
        private bool _inIsExprType;
        protected bool RelationalExpression(out ExpressionSyntax result)
        {
            if (ShiftExpression(out result))
            {
                while (true)
                {
                    SyntaxKind kind;
                    SyntaxToken st;
                    if (Token('<', out st))
                    {
                        kind = SyntaxKind.LessThanExpression;
                    }
                    else if (Token((int)TokenKind.LessThanEquals, out st))
                    {
                        kind = SyntaxKind.LessThanOrEqualExpression;
                    }
                    else if (Token('>', out st))
                    {
                        kind = SyntaxKind.GreaterThanExpression;
                    }
                    else if (Token((int)TokenKind.GreaterThanEquals, out st))
                    {
                        kind = SyntaxKind.GreaterThanOrEqualExpression;
                    }
                    else if (Keyword("is", out st))
                    {
                        kind = SyntaxKind.IsExpression;
                    }
                    else if (Keyword("as", out st))
                    {
                        kind = SyntaxKind.AsExpression;
                    }
                    else
                    {
                        break;
                    }
                    ExpressionSyntax right;
                    if (kind == SyntaxKind.IsExpression)
                    {
                        //object o = null;
                        //var x = o is int ? 1:2;
                        _inIsExprType = true;
                        right = TypeExpected();
                        _inIsExprType = false;
                    }
                    else if (kind == SyntaxKind.AsExpression)
                    {
                        right = TypeExpected();
                    }
                    else
                    {
                        if (!ShiftExpression(out right))
                        {
                            ExpressionExpectedErrorAndThrow();
                        }
                    }
                    result = SyntaxFactory.BinaryExpression(kind, result, st, right);
                }
            }
            return result != null;
        }
        protected bool ShiftExpression(out ExpressionSyntax result)
        {
            if (AdditiveExpression(out result))
            {
                while (true)
                {
                    SyntaxKind exprKind;
                    SyntaxToken st;


                    if (Token((int)TokenKind.LessThanLessThan, out st))
                    {
                        exprKind = SyntaxKind.LeftShiftExpression;
                    }
                    else if (Token('>', '>', SyntaxKind.GreaterThanGreaterThanToken, out st))
                    {
                        exprKind = SyntaxKind.RightShiftExpression;
                    }
                    else
                    {
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
                    ConsumeToken();
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
                    ConsumeToken();
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
                        ConsumeToken();
                        TypeSyntax type;
                        if (Type(out type))
                        {
                            SyntaxToken closeParen;
                            if (Token(')', out closeParen))
                            {
                                ExpressionSyntax expr;
                                if (PrefixUnaryExpression(out expr))
                                {
                                    result = SyntaxFactory.CastExpression(Token(SyntaxKind.OpenParenToken, token.Index), type, closeParen, expr);
                                    return true;
                                }
                            }
                        }
                        _tokenIndex = tkIdx;
                    }
                    break;
                case (int)TokenKind.NormalIdentifier:
                    {
                        if (token.Value == "await")
                        {
                            var tkIdx = _tokenIndex;
                            ConsumeToken();
                            ExpressionSyntax expr;
                            if (PrefixUnaryExpression(out expr))
                            {
                                result = SyntaxFactory.AwaitExpression(Token(SyntaxKind.AwaitKeyword, token.Index), expr);
                                return true;
                            }
                            _tokenIndex = tkIdx;
                        }
                    }
                    break;
            }
            if (exprKind != SyntaxKind.None)
            {
                ConsumeToken();
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
            var tokenText = token.Value;
            switch (token.Kind)
            {
                case (int)TokenKind.NormalString:
                case (int)TokenKind.VerbatimString:
                    ConsumeToken();
                    result = SyntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression,
                        SyntaxFactory.Literal(tokenText).Attach(token.Index));
                    break;
                case (int)TokenKind.Char:
                    ConsumeToken();
                    result = SyntaxFactory.LiteralExpression(SyntaxKind.CharacterLiteralExpression,
                        SyntaxFactory.Literal(tokenText[0]).Attach(token.Index));
                    break;
                case (int)TokenKind.Number:
                    ConsumeToken();
                    result = SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                        SyntaxFactory.ParseToken(tokenText).Attach(token.Index));
                    break;
                case '(':
                    ConsumeToken();
                    result = SyntaxFactory.ParenthesizedExpression(Token(SyntaxKind.OpenParenToken, token.Index), ExpressionExpected(),
                        TokenExpected(')'));
                    break;
                case (int)TokenKind.NormalIdentifier:
                    switch (tokenText)
                    {
                        case "true":
                            ConsumeToken();
                            result = SyntaxFactory.LiteralExpression(SyntaxKind.TrueLiteralExpression,
                                Token(SyntaxKind.TrueKeyword, token.Index));
                            break;
                        case "false":
                            ConsumeToken();
                            result = SyntaxFactory.LiteralExpression(SyntaxKind.FalseLiteralExpression,
                                Token(SyntaxKind.FalseKeyword, token.Index));
                            break;
                        case "null":
                            ConsumeToken();
                            result = SyntaxFactory.LiteralExpression(SyntaxKind.NullLiteralExpression,
                                Token(SyntaxKind.NullKeyword, token.Index));
                            break;
                        case "this":
                            ConsumeToken();
                            result = SyntaxFactory.ThisExpression(Token(SyntaxKind.ThisKeyword, token.Index));
                            break;
                        case "base":
                            ConsumeToken();
                            result = SyntaxFactory.BaseExpression(Token(SyntaxKind.BaseKeyword, token.Index));
                            break;
                        case "typeof":
                            ConsumeToken();
                            result = SyntaxFactory.TypeOfExpression(Token(SyntaxKind.TypeOfKeyword, token.Index),
                                TokenExpected('('), TypeExpected(), TokenExpected(')'));
                            break;
                        case "default":
                            ConsumeToken();
                            result = SyntaxFactory.DefaultExpression(Token(SyntaxKind.DefaultKeyword, token.Index),
                                TokenExpected('('), TypeExpected(), TokenExpected(')'));
                            break;
                        case "checked":
                            ConsumeToken();
                            result = SyntaxFactory.CheckedExpression(SyntaxKind.CheckedExpression, Token(SyntaxKind.CheckedKeyword, token.Index),
                                TokenExpected('('), ExpressionExpected(), TokenExpected(')'));
                            break;
                        case "unchecked":
                            ConsumeToken();
                            result = SyntaxFactory.CheckedExpression(SyntaxKind.UncheckedExpression, Token(SyntaxKind.UncheckedKeyword, token.Index),
                                TokenExpected('('), ExpressionExpected(), TokenExpected(')'));
                            break;
                        case "async":
                            {
                                var token2 = GetToken(1);
                                if (token2.IsKeyword("delegate"))
                                {
                                    ConsumeToken();
                                    result = AnonymousMethodExpression(Token(SyntaxKind.AsyncKeyword, token.Index));
                                }
                            }
                            break;
                        case "delegate":
                            result = AnonymousMethodExpression(default(SyntaxToken));
                            break;
                        case "new":
                            ConsumeToken();


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
                            ConsumeToken();
                            result = SyntaxFactory.MemberAccessExpression(SyntaxKind.SimpleMemberAccessExpression, result,
                                Token(SyntaxKind.DotToken, token.Index), SimpleNameExpected(true));
                            break;
                        case (int)TokenKind.PlusEquals:
                            ConsumeToken();
                            result = SyntaxFactory.PostfixUnaryExpression(SyntaxKind.PostIncrementExpression, result,
                                Token(SyntaxKind.PlusPlusToken, token.Index));
                            break;
                        case (int)TokenKind.MinusMinus:
                            ConsumeToken();
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
        private AnonymousMethodExpressionSyntax AnonymousMethodExpression(SyntaxToken asyncKeyword)
        {
            return SyntaxFactory.AnonymousMethodExpression(asyncKeyword, KeywordExpected("delegate"), null, null);
        }

        private static readonly HashSet<string> _predefinedTypeNameSet = new HashSet<string> {
            "sbyte","byte","short","ushort","int","uint","long","ulong","decimal","float","double","bool","string","char","object","void"
        };
        protected bool PredefinedType(out PredefinedTypeSyntax result)
        {
            SyntaxToken st;
            if (Keyword(_predefinedTypeNameSet, out st))
            {
                result = SyntaxFactory.PredefinedType(st);
                return true;
            }
            result = null;
            return false;
        }

        protected bool Type(out TypeSyntax result)
        {
            SyntaxToken st;
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
            if (PeekToken(0, '?'))
            {
                var got = true;
                if (_inConditionalExprCondition && _inIsExprType)
                {
                    got = !PeekToken(1, (int)TokenKind.NormalIdentifier, '(');//todo?
                }
                if (got)
                {
                    Token('?', out st);
                    result = SyntaxFactory.NullableType(result, st);
                }
            }
            if (PeekToken(0, '['))
            {
                result = SyntaxFactory.ArrayType(result, List(_ArrayRankSpecifierSyntaxCreator));
            }
            return true;
        }
        protected TypeSyntax TypeExpected()
        {
            TypeSyntax result;
            if (!Type(out result))
            {
                ErrorAndThrow("Type expected.");
            }
            return result;
        }
        private bool _allowArrayRankSpecifierExprs;
        protected bool ArrayRankSpecifier(out ArrayRankSpecifierSyntax result)
        {
            SyntaxToken st;
            if (Token('[', out st))
            {
                result = SyntaxFactory.ArrayRankSpecifier(st,
                    SeparatedList(_allowArrayRankSpecifierExprs ? _ExpressionSyntaxCreator : null, ',', false, _OmittedArraySizeExpressionSyntaxCreator),
                    TokenExpected(']'));
                return true;
            }
            result = null;
            return false;
        }
        protected bool NameStart(bool checkTypeArgs, out NameSyntax result)
        {
            if (PeekToken(0, (int)TokenKind.NormalIdentifier, (int)TokenKind.VerbatimIdentifier))
            {
                if (PeekToken(1, (int)TokenKind.ColonColon))
                {
                    IdentifierNameSyntax alias;
                    SyntaxToken globalToken;
                    if (Keyword("global", out globalToken))
                    {
                        alias = SyntaxFactory.IdentifierName(globalToken);
                    }
                    else
                    {
                        IdentifierName(out alias);
                    }
                    result = SyntaxFactory.AliasQualifiedName(alias, TokenExpected((int)TokenKind.ColonColon), SimpleNameExpected(checkTypeArgs));
                    return true;
                }
                SimpleNameSyntax sn;
                if (SimpleName(checkTypeArgs, out sn))
                {
                    result = sn;
                    return true;
                }
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
            while (true)
            {
                SyntaxToken dot;
                if (Token('.', out dot))
                {
                    result = SyntaxFactory.QualifiedName(result, dot, SimpleNameExpected(false));
                }
                else
                {
                    break;
                }
            }
            return true;
        }
        //protected bool AliasQualifiedName(out AliasQualifiedNameSyntax result)
        //{
        //    if (PeekToken(0, (int)TokenKind.NormalIdentifier, (int)TokenKind.VerbatimIdentifier)
        //        && PeekToken(1, (int)TokenKind.ColonColon))
        //    {
        //        IdentifierNameSyntax alias;
        //        SyntaxToken globalToken;
        //        if (Keyword(SyntaxKind.GlobalKeyword, out globalToken))
        //        {
        //            alias = SyntaxFactory.IdentifierName(globalToken);
        //        }
        //        else
        //        {
        //            IdentifierName(out alias);
        //        }
        //        SyntaxToken colonColonToken;
        //        Token((int)TokenKind.ColonColon, out colonColonToken);
        //        result = SyntaxFactory.AliasQualifiedName(alias, colonColonToken, SimpleNameExpected());
        //        return true;
        //    }
        //    result = null;
        //    return false;
        //}
        protected bool SimpleName(bool checkTypeArgs, out SimpleNameSyntax result)
        {
            var idToken = GetToken();
            if (idToken.IsIdentifier)
            {
                ConsumeToken();
                var ltToken = GetToken();
                if (ltToken.Kind == '<')
                {
                    var tkIdx = _tokenIndex;
                    ConsumeToken();
                    var typeArgs = SeparatedList(_TypeSyntaxCreator, ',', false, _OmittedTypeArgumentSyntaxCreator);
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


            //if (PeekToken(0, (int)TokenKind.NormalIdentifier, (int)TokenKind.VerbatimIdentifier))
            //{
            //    if (PeekToken(1, '<'))
            //    {
            //        GenericNameSyntax gn;
            //        GenericName(out gn);
            //        result = gn;
            //    }
            //    else
            //    {
            //        IdentifierNameSyntax i;
            //        IdentifierName(out i);
            //        result = i;
            //    }
            //    return true;
            //}
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
            SyntaxToken identifier;
            if (Identifier(out identifier))
            {
                result = SyntaxFactory.IdentifierName(identifier);
                return true;
            }
            result = null;
            return false;
        }
        //protected bool GenericName(out GenericNameSyntax result)
        //{
        //    SyntaxToken identifier;
        //    if (Identifier(out identifier))
        //    {
        //        result = SyntaxFactory.GenericName(identifier, TypeArgumentListExpected());
        //        return true;
        //    }
        //    result = null;
        //    return false;
        //}
        //protected bool TypeArgumentList(out TypeArgumentListSyntax result)
        //{
        //    SyntaxToken lessThanToken;
        //    if (Token('<', out lessThanToken))
        //    {
        //        var arguments = SeparatedList(_TypeSyntaxCreator, ',', false, _OmittedTypeArgumentSyntaxCreator);
        //        result = SyntaxFactory.TypeArgumentList(lessThanToken, arguments, TokenExpected('>'));
        //        return true;
        //    }
        //    result = null;
        //    return false;
        //}
        //protected TypeArgumentListSyntax TypeArgumentListExpected()
        //{
        //    TypeArgumentListSyntax result;
        //    if (!TypeArgumentList(out result))
        //    {
        //        ErrorAndThrow("Type argument list expected.");
        //    }
        //    return result;
        //}

        //
        //
        //

        protected bool ExternAliasDirective(out ExternAliasDirectiveSyntax result)
        {
            SyntaxToken externKeyword;
            if (Keyword("extern", out externKeyword))
            {
                var aliasKeyword = KeywordExpected("alias");
                var identifier = IdentifierExpected();
                var semicolonToken = TokenExpected(';');
                result = SyntaxFactory.ExternAliasDirective(externKeyword, aliasKeyword, identifier, semicolonToken);
                return true;
            }
            result = null;
            return false;
        }





        //protected bool Token(int kind, out Token token) {
        //    var tk = GetToken();
        //    if (tk.Kind == kind) {
        //        token = tk;
        //        ConsumeToken();
        //        return true;
        //    }
        //    token = default(Token);
        //    return false;
        //}
        //protected bool Token(int kind) {
        //    if (GetToken().Kind == kind) {
        //        ConsumeToken();
        //        return true;
        //    }
        //    return false;
        //}
        //protected bool Token(TokenKind tokenKind) {
        //    return Token((int)tokenKind);
        //}
        //protected void TokenExpected(char ch) {
        //    if (!Token(ch)) {
        //        ErrorAndThrow(ch.ToString() + " expected.");
        //    }
        //}
        //protected void TokenExpected(int kind, string errMsg) {
        //    if (!Token(kind)) {
        //        ErrorAndThrow(errMsg);
        //    }
        //}
        //protected void TokenExpected(TokenKind tokenKind, string errMsg) {
        //    TokenExpected((int)tokenKind, errMsg);
        //}
        //
        //
        //












        //
        //
        //




        //public void TokenExpected(char ch, out TextSpan textSpan) {
        //    if (!Token(ch, out textSpan)) {
        //        ErrorAndThrow(ch.ToString() + " expected.");
        //    }
        //}
        //protected void TokenExpected(int kind, string errMsg, out TextSpan textSpan) {
        //    if (!Token(kind, out textSpan)) {
        //        ErrorAndThrow(errMsg);
        //    }
        //}
        //protected void EndOfFileExpected() {
        //    TokenExpected(char.MaxValue, "End of file expected.");
        //}
        //protected bool Name(out NameNode result) {
        //    var token = GetToken();
        //    if (IsIdentifierToken(token.TokenKind)) {
        //        result = new NameNode(token.Value, GetTextSpan(token));
        //        ConsumeToken();
        //        return true;
        //    }
        //    result = default(NameNode);
        //    return false;
        //}
        //protected NameNode NameExpected() {
        //    NameNode name;
        //    if (!Name(out name)) {
        //        ErrorAndThrow("Name expected.");
        //    }
        //    return name;
        //}
        //protected bool Keyword(string keywordValue) {
        //    var token = GetToken();
        //    if (token.IsIdentifier && token.Value == keywordValue) {
        //        ConsumeToken();
        //        return true;
        //    }
        //    return false;
        //}
        //protected void KeywordExpected(string keywordValue) {
        //    if (!Keyword(keywordValue)) {
        //        ErrorAndThrow(keywordValue + " expetced.");
        //    }
        //}
        //protected bool Keyword(string keywordValue, out NameNode keyword) {
        //    var token = GetToken();
        //    if (token.TokenKind == TokenKind.Name && token.Value == keywordValue) {
        //        keyword = new NameNode(keywordValue, GetTextSpan(token));
        //        ConsumeToken();
        //        return true;
        //    }
        //    keyword = default(NameNode);
        //    return false;
        //}
        //protected bool Keyword(string keywordValue, out TextSpan textSpan) {
        //    var token = GetToken();
        //    if (token.TokenKind == TokenKind.Name && token.Value == keywordValue) {
        //        textSpan = GetTextSpan(token);
        //        ConsumeToken();
        //        return true;
        //    }
        //    textSpan = default(TextSpan);
        //    return false;
        //}
        //protected bool QName(out QNameNode result) {
        //    NameNode name;
        //    if (Name(out name)) {
        //        if (Token(TokenKind.ColonColon)) {
        //            result = new QNameNode(name, NameExpected());
        //        }
        //        else {
        //            result = new QNameNode(default(NameNode), name);
        //        }
        //        return true;
        //    }
        //    result = default(QNameNode);
        //    return false;
        //}
        //protected QNameNode QNameExpected() {
        //    QNameNode qName;
        //    if (!QName(out qName)) {
        //        ErrorAndThrow("Qualifiable name expected.");
        //    }
        //    return qName;
        //}

        //protected bool AtomValue(out AtomValueNode result, bool takeNumberSign) {
        //    var tk = GetToken();
        //    var tkKind = tk.Kind;
        //    string sign = null;
        //    if (takeNumberSign) {
        //        if (tkKind == '-') {
        //            sign = "-";
        //        }
        //        else if (tkKind == '+') {
        //            sign = "+";
        //        }
        //        if (sign != null) {
        //            if (!IsNumberToken(GetToken(1).Kind)) {
        //                result = default(AtomValueNode);
        //                return false;
        //            }
        //            ConsumeToken();
        //            tk = GetToken();
        //            tkKind = tk.Kind;
        //        }
        //    }
        //    var text = tk.Value;
        //    var typeKind = TypeKind.None;
        //    object value = null;
        //    switch ((TokenKind)tkKind) {
        //        case TokenKind.StringValue:
        //        case TokenKind.VerbatimStringValue:
        //            typeKind = TypeKind.String;
        //            value = text;
        //            break;
        //        case TokenKind.CharValue:
        //            typeKind = TypeKind.Char;
        //            value = text[0];
        //            break;
        //        case TokenKind.Name:
        //            if (text == "null") {
        //                typeKind = TypeKind.Null;
        //            }
        //            else if (text == "true") {
        //                typeKind = TypeKind.Boolean;
        //                value = true;
        //            }
        //            else if (text == "false") {
        //                typeKind = TypeKind.Boolean;
        //                value = false;
        //            }
        //            break;
        //        case TokenKind.IntegerValue: {
        //                if (sign != null) text = sign + text;
        //                int intv;
        //                if (AtomExtensions.TryInvParse(text, out intv)) {
        //                    value = intv;
        //                    typeKind = TypeKind.Int32;
        //                }
        //                else {
        //                    long longv;
        //                    if (AtomExtensions.TryInvParse(text, out longv)) {
        //                        value = longv;
        //                        typeKind = TypeKind.Int64;
        //                    }
        //                    else {
        //                        ulong ulongv;
        //                        if (AtomExtensions.TryInvParse(text, out ulongv)) {
        //                            value = ulongv;
        //                            typeKind = TypeKind.UInt64;
        //                        }
        //                        else {
        //                            ErrorAndThrow(new DiagMsg(DiagCode.InvalidAtomValue, "Int32, Int64 or UInt64", text), GetTextSpan(tk));
        //                        }
        //                    }
        //                }
        //            }
        //            break;
        //        case TokenKind.DecimalValue:
        //        case TokenKind.RealValue: {
        //                if (sign != null) text = sign + text;
        //                double doublev;
        //                if (!AtomExtensions.TryInvParse(text, out doublev)) {
        //                    ErrorAndThrow(new DiagMsg(DiagCode.InvalidAtomValue, "Double", text), GetTextSpan(tk));
        //                }
        //                value = doublev;
        //                typeKind = TypeKind.Double;
        //            }
        //            break;
        //    }
        //    if (typeKind != TypeKind.None) {
        //        result = new AtomValueNode(true, typeKind, value, text, GetTextSpan(tk));
        //        ConsumeToken();
        //        return true;
        //    }
        //    if (tkKind == '$') {
        //        ConsumeToken();
        //        var typeNameNode = NameExpected();
        //        var typeName = typeNameNode.Value;
        //        typeKind = AtomTypeMd.GetTypeKind(typeName);
        //        if (typeKind == TypeKind.None) {
        //            ErrorAndThrow("fdfd");
        //        }
        //        TokenExpected('(');
        //        tk = GetToken();
        //        text = tk.Value;
        //        tkKind = tk.Kind;
        //        switch (typeKind) {
        //            case TypeKind.String:
        //            case TypeKind.IgnoreCaseString:
        //            case TypeKind.Guid:
        //            case TypeKind.TimeSpan:
        //            case TypeKind.DateTimeOffset:
        //                if (!IsStringToken(tkKind)) {
        //                    ErrorAndThrow("String value expected.", tk);
        //                }
        //                break;
        //            case TypeKind.Char:
        //                if (tkKind != (int)TokenKind.CharValue) {
        //                    ErrorAndThrow("Char value expected.", tk);
        //                }
        //                break;
        //            case TypeKind.Boolean:
        //                if (tkKind != (int)TokenKind.Name) {
        //                    ErrorAndThrow("Name value expected.", tk);
        //                }
        //                break;
        //            case TypeKind.Decimal:
        //            case TypeKind.Int64:
        //            case TypeKind.Int32:
        //            case TypeKind.Int16:
        //            case TypeKind.SByte:
        //            case TypeKind.UInt64:
        //            case TypeKind.UInt32:
        //            case TypeKind.UInt16:
        //            case TypeKind.Byte:
        //            case TypeKind.Double:
        //            case TypeKind.Single:
        //                sign = null;
        //                if (tkKind == '-') {
        //                    sign = "-";
        //                }
        //                else if (tkKind == '+') {
        //                    sign = "+";
        //                }
        //                if (sign != null) {
        //                    ConsumeToken();
        //                    tk = GetToken();
        //                    tkKind = tk.Kind;
        //                }
        //                if (IsNumberToken(tkKind)) {
        //                    if (sign != null) text = sign + text;
        //                }
        //                else {
        //                    if (sign != null || (typeKind != TypeKind.Double && typeKind == TypeKind.Single)) {
        //                        ErrorAndThrow("Number value expected.", tk);
        //                    }
        //                    if (!IsStringToken(tkKind)) {
        //                        ErrorAndThrow("String value expected.", tk);
        //                    }
        //                }
        //                break;
        //            case TypeKind.Binary:
        //                if (tkKind == (int)TokenKind.IntegerValue) {
        //                    var bin = new Binary();
        //                    byte by;
        //                    if (!AtomExtensions.TryInvParse(text, out by)) {
        //                        ErrorAndThrow("Byte value expected.", tk);
        //                    }
        //                    bin.Add(by);
        //                    ConsumeToken();
        //                    while (Token(',')) {
        //                        tk = GetToken();
        //                        if (!AtomExtensions.TryInvParse(tk.Value, out by)) {
        //                            ErrorAndThrow("Byte value expected.", tk);
        //                        }
        //                        bin.Add(by);
        //                        ConsumeToken();
        //                    }
        //                    TokenExpected(')');
        //                    result = new AtomValueNode(false, TypeKind.Binary, bin, "$Binary(...)", typeNameNode.TextSpan);
        //                    return true;
        //                }
        //                else if (tkKind == ')') {
        //                    ConsumeToken();
        //                    result = new AtomValueNode(false, TypeKind.Binary, new Binary(), "$Binary()", typeNameNode.TextSpan);
        //                    return true;
        //                }
        //                else if (!IsStringToken(tkKind)) {
        //                    ErrorAndThrow("String value expected.", tk);
        //                }
        //                break;
        //            default:
        //                throw new ArgumentException("Invalid type kind: " + typeKind.ToString());
        //        }
        //        value = AtomExtensions.TryParse(typeKind, text);
        //        if (value == null) {
        //            ErrorAndThrow("fdf");
        //        }
        //        ConsumeToken();
        //        TokenExpected(')');
        //        result = new AtomValueNode(false, typeKind, value, text, GetTextSpan(tk));
        //        return true;
        //    }
        //    result = default(AtomValueNode);
        //    return false;
        //}
        //protected AtomValueNode AtomValueExpected(bool takeNumberSign) {
        //    AtomValueNode av;
        //    if (!AtomValue(out av, takeNumberSign)) {
        //        ErrorAndThrow("Atom value expected.");
        //    }
        //    return av;
        //}
        //protected AtomValueNode NonNullAtomValueExpected(bool takeNumberSign) {
        //    var av = AtomValueExpected(takeNumberSign);
        //    if (av.IsNull) {
        //        ErrorAndThrow("Non-null atom value expected.", av.TextSpan);
        //    }
        //    return av;
        //}
        //protected bool StringValue(out AtomValueNode result) {
        //    var token = GetToken();
        //    if (IsStringToken(token.TokenKind)) {
        //        result = new AtomValueNode(true, TypeKind.String, token.Value, token.Value, GetTextSpan(token));
        //        ConsumeToken();
        //        return true;
        //    }
        //    result = default(AtomValueNode);
        //    return false;
        //}
        //protected AtomValueNode StringValueExpected() {
        //    AtomValueNode av;
        //    if (!StringValue(out av)) {
        //        ErrorAndThrow("String value expected.");
        //    }
        //    return av;
        //}
        //protected AtomValueNode UriExpected() {
        //    var uri = StringValueExpected();
        //    if (uri.Text == Extensions.SystemUri) {
        //        ErrorAndThrow(new DiagMsg(DiagCode.UriReserved), uri.TextSpan);
        //    }
        //    return uri;
        //}
        //protected NameNode AliasExpected() {
        //    var aliasNode = NameExpected();
        //    var alias = aliasNode.Value;
        //    if (alias == "sys" || alias == "thisns") {
        //        ErrorAndThrow(new DiagMsg(DiagCode.AliasReserved), aliasNode.TextSpan);
        //    }
        //    return aliasNode;
        //}

        //
        //
        //expr
        //
        //
        //protected bool ExpressionValue(out ExpressionValue result) {
        //    AtomValueNode av;
        //    if (AtomValue(out av, true)) {
        //        result = new AtomExpressionValue(av.TextSpan, av.Kind, av.Value, av.IsImplicit);
        //        return true;
        //    }
        //    TextSpan ts;
        //    if (Token('[', out ts)) {
        //        List<ExpressionValue> items = null;
        //        while (true) {
        //            if (items != null) {
        //                if (!Token(',')) {
        //                    break;
        //                }
        //            }
        //            ExpressionValue item;
        //            if (ExpressionValue(out item)) {
        //                if (items == null) {
        //                    items = new List<ExpressionValue>();
        //                }
        //                items.Add(item);
        //            }
        //            else {
        //                break;
        //            }
        //        }
        //        TokenExpected(']');
        //        result = new ListExpressionValue(ts, items);
        //        return true;
        //    }
        //    if (Token('{', out ts)) {
        //        List<NamedExpressionValue> members = null;
        //        while (true) {
        //            if (members != null) {
        //                if (!Token(',')) {
        //                    break;
        //                }
        //            }
        //            NameNode nameNode;
        //            if (Name(out nameNode)) {
        //                var name = nameNode.Value;
        //                if (members != null) {
        //                    foreach (var item in members) {
        //                        if (item.Name == name) {
        //                            ErrorAndThrow("fdsf");
        //                        }
        //                    }
        //                }
        //                TokenExpected('=');
        //                if (members == null) {
        //                    members = new List<NamedExpressionValue>();
        //                }
        //                members.Add(new NamedExpressionValue(name, ExpressionValueExpected()));
        //            }
        //            else {
        //                break;
        //            }
        //        }
        //        TokenExpected('}');
        //        result = new ObjectExpressionValue(ts, members);
        //        return true;
        //    }
        //    result = null;
        //    return false;
        //}
        //protected ExpressionValue ExpressionValueExpected() {
        //    ExpressionValue v;
        //    if (!ExpressionValue(out v)) {
        //        ErrorAndThrow("Expression value expected.");
        //    }
        //    return v;
        //}

        //protected List<NamedExpressionValue> ExpressionArguments() {
        //    List<NamedExpressionValue> list = null;
        //    NameNode nameNode;
        //    while (Name(out nameNode)) {
        //        var name = nameNode.Value;
        //        if (list == null) {
        //            list = new List<NamedExpressionValue>();
        //        }
        //        else {
        //            foreach (var item in list) {
        //                if (item.Name == name) {
        //                    ErrorAndThrow(new DiagMsg(DiagCode.DuplicateExpressionArgumentName, name), nameNode.TextSpan);
        //                }
        //            }
        //        }
        //        TokenExpected('=');
        //        list.Add(new NamedExpressionValue(name, ExpressionValueExpected()));
        //    }
        //    return list;
        //}
        //protected ExpressionNode Expression(ExpressionContext ctx) {
        //    var uriAliasList = ctx.UriAliasList;
        //    while (Token('#')) {
        //        KeywordExpected(ParserKeywords.ImportKeyword);
        //        var uriNode = UriExpected();
        //        var uri = uriNode.Text;
        //        if (!ProgramMd.IsUriDefined(uri)) {
        //            ErrorAndThrow(new DiagMsg(DiagCode.InvalidUriReference, uri), uriNode.TextSpan);
        //        }
        //        string alias = null;
        //        if (Keyword(ParserKeywords.AsKeyword)) {
        //            var aliasNode = AliasExpected();
        //            alias = aliasNode.Value;
        //            if (uriAliasList.Count > 0) {
        //                foreach (var item in uriAliasList) {
        //                    if (item.Alias == alias) {
        //                        ErrorAndThrow(new DiagMsg(DiagCode.DuplicateAlias, alias), aliasNode.TextSpan);
        //                    }
        //                }
        //            }
        //        }
        //        uriAliasList.Add(new UriAliasNode(uri, alias));
        //    }
        //    return ExpressionExpected(ctx);
        //}
        //private void ExpressionExpectedErrorAndThrow() {
        //    ErrorAndThrow("Expression expected.");
        //}
        //private ExpressionNode ExpressionExpected(ExpressionContext ctx) {
        //    ExpressionNode r;
        //    if (!Expression(ctx, out r)) {
        //        ExpressionExpectedErrorAndThrow();
        //    }
        //    return r;
        //}
        //private NameNode LambdaParameterName(ExpressionContext ctx) {
        //    var nameNode = NameExpected();
        //    ctx.CheckLambdaParameterName(nameNode);
        //    return nameNode;
        //}
        //private bool Expression(ExpressionContext ctx, out ExpressionNode result) {
        //    object lambdaParaOrList = null;
        //    var tk0Kind = GetToken().Kind;
        //    if (IsIdentifierToken(tk0Kind)) {
        //        if (GetToken(1).TokenKind == TokenKind.EqualsGreaterThan) {
        //            var nameNode = LambdaParameterName(ctx);
        //            //lambdaParaOrList = new LambdaParameterNode();
        //        }
        //    }
        //    else if (tk0Kind == '(') {
        //        var tk1Kind = GetToken(1).Kind;
        //        if (IsIdentifierToken(tk1Kind)) {
        //            var tk2Kind = GetToken(2).Kind;
        //            if (tk2Kind == ')') {
        //                if (GetToken(3).TokenKind == TokenKind.EqualsGreaterThan) {
        //                    ConsumeToken();// (
        //                    var nameNode = LambdaParameterName(ctx);
        //                    //lambdaParaOrList = new LambdaParameterNode(NameExpected());
        //                    ConsumeToken();// )
        //                }
        //            }
        //            else if (tk2Kind == ',') {
        //                ConsumeToken();// (
        //                var nameNode = LambdaParameterName(ctx);
        //                var list = new List<LambdaParameterNode> { new LambdaParameterNode(nameNode.Value, null) };
        //                while (Token(',')) {
        //                    nameNode = LambdaParameterName(ctx);
        //                    var name = nameNode.Value;
        //                    foreach (var item in list) {
        //                        if (item.Name == name) {
        //                            ErrorAndThrow(new DiagMsg(DiagCode.DuplicateLambdaParameterName, name), nameNode.TextSpan);
        //                        }
        //                    }
        //                    list.Add(new LambdaParameterNode(nameNode.Value, null));
        //                }
        //                TokenExpected(')');
        //                lambdaParaOrList = list;
        //            }
        //        }
        //        else if (tk1Kind == ')') {
        //            ConsumeToken();// (
        //            ConsumeToken();// )
        //            //lambdaParaOrList = new LambdaParameterNodeList();
        //        }
        //    }
        //    if (lambdaParaOrList != null) {
        //        TextSpan ts;
        //        TokenExpected((int)TokenKind.EqualsGreaterThan, "=> expected.", out ts);
        //        if (lambdaParaOrList != null) {
        //            ctx.PushLambdaParameter(lambdaParaOrList);
        //        }
        //        var body = ExpressionExpected(ctx);
        //        if (lambdaParaOrList != null) {
        //            ctx.PopLambdaParameter();
        //        }
        //        result = new LambdaExpressionNode(ts, null, lambdaParaOrList, body);
        //        return true;
        //    }
        //    return ConditionalExpression(ctx, out result);
        //}
        //private bool ConditionalExpression(ExpressionContext ctx, out ExpressionNode result) {
        //    ExpressionNode condition;
        //    if (CoalesceExpression(ctx, out condition)) {
        //        TextSpan ts;
        //        ExpressionNode whenTrue = null, whenFalse = null;
        //        if (Token('?', out ts)) {
        //            whenTrue = ExpressionExpected(ctx);
        //            TokenExpected(':');
        //            whenFalse = ExpressionExpected(ctx);
        //        }
        //        if (whenTrue == null) {
        //            result = condition;
        //        }
        //        else {
        //            result = new ConditionalExpressionNode(ts, null, condition, whenTrue, whenFalse);
        //        }
        //        return true;
        //    }
        //    result = null;
        //    return false;
        //}
        //private bool CoalesceExpression(ExpressionContext ctx, out ExpressionNode result) {
        //    ExpressionNode left;
        //    if (OrElseExpression(ctx, out left)) {
        //        TextSpan ts;
        //        ExpressionNode right = null;
        //        if (Token((int)TokenKind.QuestionQuestion, out ts)) {
        //            if (!CoalesceExpression(ctx, out right)) {
        //                ExpressionExpectedErrorAndThrow();
        //            }
        //        }
        //        if (right == null) {
        //            result = left;
        //        }
        //        else {
        //            result = new BinaryExpressionNode(ts, ExpressionKind.Coalesce, null, left, right);
        //        }
        //        return true;
        //    }
        //    result = null;
        //    return false;
        //}
        //private bool OrElseExpression(ExpressionContext ctx, out ExpressionNode result) {
        //    if (AndAlsoExpression(ctx, out result)) {
        //        TextSpan ts;
        //        while (Token((int)TokenKind.BarBar, out ts)) {
        //            ExpressionNode right;
        //            if (!AndAlsoExpression(ctx, out right)) {
        //                ExpressionExpectedErrorAndThrow();
        //            }
        //            result = new BinaryExpressionNode(ts, ExpressionKind.OrElse, null, result, right);
        //        }
        //    }
        //    return result != null;
        //}
        //private bool AndAlsoExpression(ExpressionContext ctx, out ExpressionNode result) {
        //    if (OrExpression(ctx, out result)) {
        //        TextSpan ts;
        //        while (Token((int)TokenKind.AmpersandAmpersand, out ts)) {
        //            ExpressionNode right;
        //            if (!OrExpression(ctx, out right)) {
        //                ExpressionExpectedErrorAndThrow();
        //            }
        //            result = new BinaryExpressionNode(ts, ExpressionKind.AndAlso, null, result, right);
        //        }
        //    }
        //    return result != null;
        //}
        //private bool OrExpression(ExpressionContext ctx, out ExpressionNode result) {
        //    if (ExclusiveOrExpression(ctx, out result)) {
        //        TextSpan ts;
        //        while (Token('|', out ts)) {
        //            ExpressionNode right;
        //            if (!ExclusiveOrExpression(ctx, out right)) {
        //                ExpressionExpectedErrorAndThrow();
        //            }
        //            result = new BinaryExpressionNode(ts, ExpressionKind.Or, null, result, right);
        //        }
        //    }
        //    return result != null;
        //}
        //private bool ExclusiveOrExpression(ExpressionContext ctx, out ExpressionNode result) {
        //    if (AndExpression(ctx, out result)) {
        //        TextSpan ts;
        //        while (Token('^', out ts)) {
        //            ExpressionNode right;
        //            if (!AndExpression(ctx, out right)) {
        //                ExpressionExpectedErrorAndThrow();
        //            }
        //            result = new BinaryExpressionNode(ts, ExpressionKind.ExclusiveOr, null, result, right);
        //        }
        //    }
        //    return result != null;
        //}
        //private bool AndExpression(ExpressionContext ctx, out ExpressionNode result) {
        //    if (EqualityExpression(ctx, out result)) {
        //        TextSpan ts;
        //        while (Token('&', out ts)) {
        //            ExpressionNode right;
        //            if (!EqualityExpression(ctx, out right)) {
        //                ExpressionExpectedErrorAndThrow();
        //            }
        //            result = new BinaryExpressionNode(ts, ExpressionKind.And, null, result, right);
        //        }
        //    }
        //    return result != null;
        //}
        //private bool EqualityExpression(ExpressionContext ctx, out ExpressionNode result) {
        //    if (RelationalExpression(ctx, out result)) {
        //        while (true) {
        //            TextSpan ts;
        //            ExpressionKind kind;
        //            if (Token((int)TokenKind.EqualsEquals, out ts)) {
        //                kind = ExpressionKind.Equal;
        //            }
        //            else if (Token((int)TokenKind.ExclamationEquals, out ts)) {
        //                kind = ExpressionKind.NotEqual;
        //            }
        //            else {
        //                break;
        //            }
        //            ExpressionNode right;
        //            if (!RelationalExpression(ctx, out right)) {
        //                ExpressionExpectedErrorAndThrow();
        //            }
        //            result = new BinaryExpressionNode(ts, kind, null, result, right);
        //        }
        //    }
        //    return result != null;
        //}
        //private bool RelationalExpression(ExpressionContext ctx, out ExpressionNode result) {
        //    if (ShiftExpression(ctx, out result)) {
        //        while (true) {
        //            TextSpan ts;
        //            ExpressionKind kind;
        //            if (Token('<', out ts)) {
        //                kind = ExpressionKind.LessThan;
        //            }
        //            else if (Token((int)TokenKind.LessThanEquals, out ts)) {
        //                kind = ExpressionKind.LessThanOrEqual;
        //            }
        //            else if (Token('>', out ts)) {
        //                kind = ExpressionKind.GreaterThan;
        //            }
        //            else if (Token((int)TokenKind.GreaterThanEquals, out ts)) {
        //                kind = ExpressionKind.GreaterThanOrEqual;
        //            }
        //            else if (Keyword(ParserKeywords.IsKeyword, out ts)) {
        //                kind = ExpressionKind.TypeIs;
        //            }
        //            else if (Keyword(ParserKeywords.AsKeyword, out ts)) {
        //                kind = ExpressionKind.TypeAs;
        //            }
        //            else {
        //                break;
        //            }
        //            if (kind == ExpressionKind.TypeIs || kind == ExpressionKind.TypeAs) {
        //                var qName = QNameExpected();
        //                result = new TypedExpressionNode(qName.TextSpan, kind, null, ctx.ResolveAsGlobalType(qName), result);
        //            }
        //            else {
        //                ExpressionNode right;
        //                if (!ShiftExpression(ctx, out right)) {
        //                    ExpressionExpectedErrorAndThrow();
        //                }
        //                result = new BinaryExpressionNode(ts, kind, null, result, right);
        //            }
        //        }
        //    }
        //    return result != null;
        //}
        //private bool ShiftExpression(ExpressionContext ctx, out ExpressionNode result) {
        //    if (AdditiveExpression(ctx, out result)) {
        //        while (true) {
        //            TextSpan ts;
        //            var kind = ExpressionKind.None;
        //            if (Token((int)TokenKind.LessThanLessThan, out ts)) {
        //                kind = ExpressionKind.LeftShift;
        //            }
        //            else {
        //                var tk0 = GetToken();
        //                if (tk0.Kind == '>') {
        //                    var tk1 = GetToken(1);
        //                    if (tk1.Kind == '>') {
        //                        if (tk0.StartIndex + 1 == tk1.StartIndex) {
        //                            ConsumeToken();
        //                            ConsumeToken();
        //                            ts = GetTextSpan(tk1);
        //                            kind = ExpressionKind.RightShift;
        //                        }
        //                    }
        //                }
        //            }
        //            if (kind == ExpressionKind.None) {
        //                break;
        //            }
        //            ExpressionNode right;
        //            if (!AdditiveExpression(ctx, out right)) {
        //                ExpressionExpectedErrorAndThrow();
        //            }
        //            result = new BinaryExpressionNode(ts, kind, null, result, right);
        //        }
        //    }
        //    return result != null;
        //}
        //private bool AdditiveExpression(ExpressionContext ctx, out ExpressionNode result) {
        //    if (MultiplicativeExpression(ctx, out result)) {
        //        while (true) {
        //            TextSpan ts;
        //            ExpressionKind kind;
        //            if (Token('+', out ts)) {
        //                kind = ExpressionKind.Add;
        //            }
        //            else if (Token('-', out ts)) {
        //                kind = ExpressionKind.Subtract;
        //            }
        //            else {
        //                break;
        //            }
        //            ExpressionNode right;
        //            if (!MultiplicativeExpression(ctx, out right)) {
        //                ExpressionExpectedErrorAndThrow();
        //            }
        //            result = new BinaryExpressionNode(ts, kind, null, result, right);
        //        }
        //    }
        //    return result != null;
        //}
        //private bool MultiplicativeExpression(ExpressionContext ctx, out ExpressionNode result) {
        //    if (PrefixUnaryExpression(ctx, out result)) {
        //        while (true) {
        //            TextSpan ts;
        //            ExpressionKind kind;
        //            if (Token('*', out ts)) {
        //                kind = ExpressionKind.Multiply;
        //            }
        //            else if (Token('/', out ts)) {
        //                kind = ExpressionKind.Divide;
        //            }
        //            else if (Token('%', out ts)) {
        //                kind = ExpressionKind.Modulo;
        //            }
        //            else {
        //                break;
        //            }
        //            ExpressionNode right;
        //            if (!PrefixUnaryExpression(ctx, out right)) {
        //                ExpressionExpectedErrorAndThrow();
        //            }
        //            result = new BinaryExpressionNode(ts, kind, null, result, right);
        //        }
        //    }
        //    return result != null;
        //}
        //private bool PrefixUnaryExpression(ExpressionContext ctx, out ExpressionNode result) {
        //    var kind = ExpressionKind.None;
        //    var tk0 = GetToken();
        //    var tk0Kind = tk0.Kind;
        //    if (tk0Kind == '!') {
        //        kind = ExpressionKind.Not;
        //    }
        //    else if (tk0Kind == '-') {
        //        kind = ExpressionKind.Negate;
        //    }
        //    else if (tk0Kind == '+') {
        //        kind = ExpressionKind.UnaryPlus;
        //    }
        //    else if (tk0Kind == '~') {
        //        kind = ExpressionKind.OnesComplement;
        //    }
        //    if (kind != ExpressionKind.None) {
        //        ConsumeToken();
        //        ExpressionNode expr;
        //        if (!PrefixUnaryExpression(ctx, out expr)) {
        //            ExpressionExpectedErrorAndThrow();
        //        }
        //        result = new UnaryExpressionNode(GetTextSpan(tk0), kind, null, expr);
        //        return true;
        //    }
        //    if (tk0Kind == '(') {
        //        var tk1Kind = GetToken(1).Kind;
        //        if (IsIdentifierToken(tk1Kind)) {
        //            var isOk = false;
        //            var tk2Kind = GetToken(2).Kind;
        //            if (tk2Kind == ')') {
        //                var tk3Kind = GetToken(3).Kind;
        //                isOk = tk3Kind != '-' && tk3Kind != '+';
        //            }
        //            else if (tk2Kind == (int)TokenKind.ColonColon) {
        //                var tk3Kind = GetToken(3).Kind;
        //                if (IsIdentifierToken(tk3Kind)) {
        //                    var tk4Kind = GetToken(4).Kind;
        //                    if (tk4Kind == ')') {
        //                        var tk5Kind = GetToken(5).Kind;
        //                        isOk = tk5Kind != '-' && tk5Kind != '+';
        //                    }
        //                }
        //            }
        //            if (isOk) {
        //                ConsumeToken();// (
        //                var qName = QNameExpected();
        //                ConsumeToken();// )
        //                ExpressionNode expr;
        //                if (PrefixUnaryExpression(ctx, out expr)) {
        //                    result = new TypedExpressionNode(qName.TextSpan, ExpressionKind.Convert, null, ctx.ResolveAsGlobalType(qName), expr);
        //                    return true;
        //                }
        //                result = new QualifiableNameExpressionNode(qName.TextSpan, ctx.Resolve(qName));
        //                return true;
        //            }
        //        }
        //    }
        //    return PrimaryExpression(ctx, out result);
        //}
        //private bool PrimaryExpression(ExpressionContext ctx, out ExpressionNode result) {
        //    ExpressionNode expr = null;
        //    TextSpan ts;
        //    AtomValueNode av;
        //    if (AtomValue(out av, false)) {
        //        if (av.IsNull) {
        //            expr = new LiteralExpressionNode(av.TextSpan, NullTypeMd.Instance, null);
        //        }
        //        else {
        //            expr = new LiteralExpressionNode(av.TextSpan, AtomTypeMd.Get(av.Kind), av.Value);
        //        }
        //    }
        //    else if (Token('#')) {
        //        var argName = NameExpected();

        //    }
        //    else if (Token('(')) {//Parenthesized
        //        var op = ExpressionExpected(ctx);
        //        TokenExpected(')');
        //        expr = op;
        //    }
        //    else if (Keyword(ParserKeywords.NewKeyword, out ts)) {
        //        TokenExpected('{');
        //        List<NamedExpressionNode> members = null;
        //        while (true) {
        //            if (members != null) {
        //                if (!Token(',')) {
        //                    break;
        //                }
        //            }
        //            NameNode nameNode;
        //            if (Name(out nameNode)) {
        //                var name = nameNode.Value;
        //                if (members != null) {
        //                    foreach (var item in members) {
        //                        if (item.Name == name) {
        //                            ErrorAndThrow("fdsf");
        //                        }
        //                    }
        //                }
        //                TokenExpected('=');
        //                if (members == null) {
        //                    members = new List<NamedExpressionNode>();
        //                }
        //                members.Add(new NamedExpressionNode(name, ExpressionExpected(ctx)));
        //            }
        //            else {
        //                break;
        //            }
        //        }
        //        TokenExpected('}');
        //        expr = new ObjectExpressionNode(ts, null, members);
        //    }
        //    else {
        //        QNameNode qName;
        //        if (QName(out qName)) {
        //            expr = new QualifiableNameExpressionNode(qName.TextSpan, ctx.Resolve(qName));
        //        }
        //    }
        //    if (expr != null) {
        //        while (true) {
        //            if (Token('.')) {
        //                var nameNode = NameExpected();
        //                var name = nameNode.Value;
        //                expr = new MemberAccessExpressionNode(nameNode.TextSpan, null, expr, name);
        //            }
        //            else if (Token('(', out ts)) {
        //                var argList = new List<ExpressionNode>();
        //                ExpressionNode arg;
        //                if (Expression(ctx, out arg)) {
        //                    argList.Add(arg);
        //                    while (Token(',')) {
        //                        argList.Add(ExpressionExpected(ctx));
        //                    }
        //                }
        //                TokenExpected(')');
        //                expr = new CallOrIndexExpressionNode(ts, null, true, expr, argList);
        //            }
        //            else if (Token('[', out ts)) {
        //                var argList = new List<ExpressionNode>();
        //                argList.Add(ExpressionExpected(ctx));
        //                while (Token(',')) {
        //                    argList.Add(ExpressionExpected(ctx));
        //                }
        //                TokenExpected(']');
        //                expr = new CallOrIndexExpressionNode(ts, null, false, expr, argList);
        //            }
        //            else {
        //                break;
        //            }
        //        }
        //    }
        //    result = expr;
        //    return expr != null;
        //}


    }
    //internal sealed class Parser : ParserBase {
    //    internal static bool ParseData(string filePath, TextReader reader, DiagContext diagCtx, ClassTypeMd classMd, out object result) {
    //        if (classMd == null) throw new ArgumentNullException("classMd");
    //        return Instance.Data(filePath, reader, diagCtx, classMd, out result);
    //    }
    //    internal static bool ParseExpressionArguments(string filePath, TextReader reader, DiagContext diagCtx, out List<NamedExpressionValue> result) {
    //        return Instance.ExpressionArguments(filePath, reader, diagCtx, out result);
    //    }
    //    internal static bool ParseExpression(string filePath, TextReader reader, DiagContext diagCtx, ExpressionContext exprCtx, out ExpressionNode result) {
    //        return Instance.Expression(filePath, reader, diagCtx, exprCtx, out result);
    //    }
    //    [ThreadStatic]
    //    private static Parser _instance;
    //    private static Parser Instance {
    //        get { return _instance ?? (_instance = new Parser()); }
    //    }
    //    private Stack<List<UriAliasNode>> _uriAliasListStack;
    //    private Parser() { }
    //    private bool Data(string filePath, TextReader reader, DiagContext diagCtx, ClassTypeMd clsMd, out object result) {
    //        try {
    //            Set(filePath, reader, diagCtx);
    //            if (_uriAliasListStack == null) {
    //                _uriAliasListStack = new Stack<List<UriAliasNode>>();
    //            }
    //            else {
    //                _uriAliasListStack.Clear();
    //            }
    //            object obj;
    //            if (ClassValue(clsMd, out obj)) {
    //                EndOfFileExpected();
    //                result = obj;
    //                return true;
    //            }
    //            else {
    //                ErrorAndThrow("Class value expected.");
    //            }
    //        }
    //        catch (DiagContext.DiagException) { }
    //        finally {
    //            Clear();
    //        }
    //        result = null;
    //        return false;
    //    }
    //    private bool ExpressionArguments(string filePath, TextReader reader, DiagContext diagCtx, out List<NamedExpressionValue> result) {
    //        try {
    //            Set(filePath, reader, diagCtx);
    //            var list = ExpressionArguments();
    //            EndOfFileExpected();
    //            result = list;
    //            return true;
    //        }
    //        catch (DiagContext.DiagException) { }
    //        finally {
    //            Clear();
    //        }
    //        result = null;
    //        return false;
    //    }
    //    private bool Expression(string filePath, TextReader reader, DiagContext diagCtx, ExpressionContext exprCtx, out ExpressionNode result) {
    //        try {
    //            Set(filePath, reader, diagCtx);
    //            var expr = Expression(exprCtx);
    //            EndOfFileExpected();
    //            result = expr;
    //            return true;
    //        }
    //        catch (DiagContext.DiagException) { }
    //        finally {
    //            Clear();
    //        }
    //        result = null;
    //        return false;
    //    }

    //    private bool UriAliasList() {
    //        if (Token('<')) {
    //            List<UriAliasNode> list = null;
    //            while (true) {
    //                NameNode aliasNode;
    //                if (Name(out aliasNode)) {
    //                    var alias = aliasNode.Value;
    //                    if (list == null) {
    //                        list = new List<UriAliasNode>();
    //                    }
    //                    else {
    //                        foreach (var item in list) {
    //                            if (item.Alias == alias) {
    //                                ErrorAndThrow(new DiagMsg(DiagCode.DuplicateAlias, alias), aliasNode.TextSpan);
    //                            }
    //                        }
    //                    }
    //                    TokenExpected('=');
    //                    list.Add(new UriAliasNode(StringValueExpected().Text, alias));
    //                }
    //                else {
    //                    TokenExpected('>');
    //                    if (list != null) {
    //                        _uriAliasListStack.Push(list);
    //                        return true;
    //                    }
    //                    return false;
    //                }
    //            }
    //        }
    //        return false;
    //    }
    //    private string GetUri(NameNode aliasNode) {
    //        var alias = aliasNode.Value;
    //        foreach (var uaList in _uriAliasListStack) {
    //            foreach (var ua in uaList) {
    //                if (ua.Alias == alias) {
    //                    return ua.Uri;
    //                }
    //            }
    //        }
    //        ErrorAndThrow(new DiagMsg(DiagCode.InvalidUriReference, alias), aliasNode.TextSpan);
    //        return null;
    //    }
    //    private bool ClassValue(ClassTypeMd declaredClsMd, out object result) {
    //        NameNode aliasNode;
    //        if (Name(out aliasNode)) {
    //            TokenExpected(':');
    //            var nameNode = NameExpected();
    //            var hasUriAliasingList = UriAliasList();
    //            TokenExpected('{');
    //            var fullName = new FullName(GetUri(aliasNode), nameNode.Value);
    //            var clsMd = ProgramMd.GetGlobalType<ClassTypeMd>(fullName);
    //            if (clsMd == null) {
    //                ErrorAndThrow(new DiagMsg(DiagCode.InvalidClassReference, fullName.ToString()), nameNode.TextSpan);
    //            }
    //            if (!clsMd.IsEqualToOrDeriveFrom(declaredClsMd)) {
    //                ErrorAndThrow(new DiagMsg(DiagCode.ClassNotEqualToOrDeriveFromTheDeclared, fullName.ToString(), declaredClsMd.FullName.ToString()),
    //                    nameNode.TextSpan);
    //            }
    //            if (clsMd.IsAbstract) {
    //                ErrorAndThrow(new DiagMsg(DiagCode.ClassIsAbstract, fullName.ToString()), nameNode.TextSpan);
    //            }
    //            var obj = clsMd.CreateInstance();
    //            clsMd.SetTextSpan(obj, nameNode.TextSpan);
    //            if (!clsMd.InvokeOnLoad(true, obj, _diagCtx)) {
    //                Throw();
    //            }
    //            List<ClassTypePropertyMd> propMdList = null;
    //            clsMd.GetPropertiesInHierarchy(ref propMdList);
    //            while (true) {
    //                NameNode propNameNode;
    //                if (Name(out propNameNode)) {
    //                    var propName = propNameNode.Value;
    //                    ClassTypePropertyMd propMd = null;
    //                    if (propMdList != null) {
    //                        for (var i = 0; i < propMdList.Count; ++i) {
    //                            if (propMdList[i].Name == propName) {
    //                                propMd = propMdList[i];
    //                                propMdList.RemoveAt(i);
    //                                break;
    //                            }
    //                        }
    //                    }
    //                    if (propMd == null) {
    //                        ErrorAndThrow(new DiagMsg(DiagCode.InvalidPropertyName, propName), propNameNode.TextSpan);
    //                    }
    //                    TokenExpected('=');
    //                    propMd.SetValue(obj, LocalValueExpected(propMd.Type));
    //                }
    //                else {
    //                    TextSpan ts;
    //                    TokenExpected('}', out ts);
    //                    if (propMdList != null && propMdList.Count > 0) {
    //                        foreach (var propMd in propMdList) {
    //                            Error(new DiagMsg(DiagCode.PropertyMissing, propMd.Name), ts);
    //                        }
    //                        Throw();
    //                    }
    //                    if (!clsMd.InvokeOnLoad(false, obj, _diagCtx)) {
    //                        Throw();
    //                    }
    //                    if (hasUriAliasingList) {
    //                        _uriAliasListStack.Pop();
    //                    }
    //                    result = obj;
    //                    return true;
    //                }
    //            }
    //        }
    //        result = null;
    //        return false;
    //    }
    //    private object LocalValueExpected(LocalTypeMd typeMd) {
    //        object value;
    //        if (LocalValue(typeMd, out value)) {
    //            return value;
    //        }
    //        ErrorAndThrow(new DiagMsg(DiagCode.ValueExpected));
    //        return null;
    //    }
    //    private bool LocalValue(LocalTypeMd typeMd, out object result) {
    //        var typeKind = typeMd.Kind;
    //        AtomValueNode avNode;
    //        if (AtomValue(out avNode, true)) {
    //            if (avNode.IsNull) {
    //                if (false) {//!typeMd.IsNullable) {
    //                    ErrorAndThrow(new DiagMsg(DiagCode.NullNotAllowed), avNode.TextSpan);
    //                }
    //                result = null;
    //                return true;
    //            }
    //            if (!typeKind.IsAtom()) {
    //                ErrorAndThrow(new DiagMsg(DiagCode.SpecificValueExpected, typeKind.ToString()), avNode.TextSpan);
    //            }
    //            result = AtomExtensions.TryParse(typeKind, avNode.Text);
    //            if (result == null) {
    //                ErrorAndThrow(new DiagMsg(DiagCode.InvalidAtomValue, typeKind.ToString(), avNode.Text), avNode.TextSpan);
    //            }
    //            return true;
    //        }
    //        else {
    //            TextSpan ts;
    //            if (Token('$', out ts)) {
    //                if (typeKind != TypeKind.Enum) {
    //                    ErrorAndThrow(new DiagMsg(DiagCode.SpecificValueExpected, typeKind.ToString()), ts);
    //                }
    //                var uri = GetUri(NameExpected());
    //                TokenExpected(':');
    //                var nameNode = NameExpected();
    //                var fullName = new FullName(uri, nameNode.Value);
    //                var enumMd = ProgramMd.GetGlobalType<EnumTypeMd>(fullName);
    //                if (enumMd == null) {
    //                    ErrorAndThrow(new DiagMsg(DiagCode.InvalidEnumReference, fullName.ToString()), nameNode.TextSpan);
    //                }
    //                var declaredEnumMd = ((GlobalTypeRefMd)typeMd).GlobalType as EnumTypeMd;
    //                if (enumMd != declaredEnumMd) {
    //                    ErrorAndThrow(new DiagMsg(DiagCode.EnumNotEqualToTheDeclared, fullName.ToString(), declaredEnumMd.FullName.ToString()),
    //                        nameNode.TextSpan);
    //                }
    //                TokenExpected('.');
    //                var memberNameNode = NameExpected();
    //                result = enumMd.GetPropertyValue(memberNameNode.Value);
    //                if (result == null) {
    //                    ErrorAndThrow(new DiagMsg(DiagCode.InvalidEnumMemberName, memberNameNode.Value), memberNameNode.TextSpan);
    //                }
    //                return true;
    //            }
    //            else if (Token('[', out ts)) {
    //                var isList = typeKind == TypeKind.List;
    //                var isSet = typeKind == TypeKind.SimpleSet || typeKind == TypeKind.ObjectSet;
    //                if (!(isList || isSet)) {
    //                    ErrorAndThrow(new DiagMsg(DiagCode.SpecificValueExpected, typeKind.ToString()), ts);
    //                }
    //                var collMd = (CollectionTypeMd)typeMd;
    //                var collObj = collMd.CreateInstance();
    //                var itemMd = collMd.ItemType;
    //                while (true) {
    //                    object itemObj;
    //                    if (isSet) {
    //                        ts = GetTextSpan();
    //                    }
    //                    if (LocalValue(itemMd, out itemObj)) {
    //                        if (isSet) {
    //                            if (!collMd.InvokeBoolAdd(collObj, itemObj)) {
    //                                ErrorAndThrow(new DiagMsg(DiagCode.DuplicateSetItem), ts);
    //                            }
    //                        }
    //                        else {
    //                            collMd.InvokeAdd(collObj, itemObj);
    //                        }
    //                    }
    //                    else {
    //                        TokenExpected(']');
    //                        result = collObj;
    //                        return true;
    //                    }
    //                }
    //            }
    //            else if (Token((int)TokenKind.HashOpenBracket, out ts)) {
    //                if (typeKind != TypeKind.Map) {
    //                    ErrorAndThrow(new DiagMsg(DiagCode.SpecificValueExpected, typeKind.ToString()), ts);
    //                }
    //                var collMd = (CollectionTypeMd)typeMd;
    //                var collObj = collMd.CreateInstance();
    //                var keyMd = collMd.MapKeyType;
    //                var valueMd = collMd.ItemType;
    //                while (true) {
    //                    object keyObj;
    //                    ts = GetTextSpan();
    //                    if (LocalValue(keyMd, out keyObj)) {
    //                        if (collMd.InvokeContainsKey(collObj, keyObj)) {
    //                            ErrorAndThrow(new DiagMsg(DiagCode.DuplicateMapKey), ts);
    //                        }
    //                        TokenExpected('=');
    //                        collMd.InvokeAdd(collObj, keyObj, LocalValueExpected(valueMd));
    //                    }
    //                    else {
    //                        TokenExpected(']');
    //                        result = collObj;
    //                        return true;
    //                    }
    //                }
    //            }
    //            else if (PeekToken((int)TokenKind.Name, (int)TokenKind.VerbatimName)) {
    //                if (typeKind != TypeKind.Class) {
    //                    ErrorAndThrow(new DiagMsg(DiagCode.SpecificValueExpected, typeKind.ToString()));
    //                }
    //                return ClassValue(((GlobalTypeRefMd)typeMd).GlobalType as ClassTypeMd, out result);
    //            }
    //        }
    //        result = null;
    //        return false;
    //    }


    //}

}

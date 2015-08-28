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
        protected ParserBase()
        {
            _ExternAliasDirectiveCreator = ExternAliasDirective;
            _UsingDirectiveCreator = UsingDirective;
            _GlobalAttributeListCreator = GlobalAttributeList;
            _AttributeListCreator = AttributeList;
            _AttributeCreator = Attribute;
            _AttributeArgumentCreator = AttributeArgument;
            _NamespaceMemberCreator = NamespaceMember;

            _ModifierCreator = Modifier;
            _TypeParameterCreator = TypeParameter;
            _BaseTypeCreator = BaseType;
            _TypeParameterConstraintCreator = TypeParameterConstraint;

            //
            _StatementCreator = Statement;
            _SwitchSectionCreator = SwitchSection;
            _SwitchLabelCreator = SwitchLabel;
            _CatchClauseCreator = CatchClause;
            _ConstVariableDeclaratorCreator = ConstVariableDeclarator;
            _VariableDeclaratorCreator = VariableDeclarator;

            //
            _ExpressionCreator = Expression;
            _ArrayInitializerExpressionCreator = ArrayInitializerExpression;
            _TypeCreator = Type;
            _ArrayRankSpecifierCreator = ArrayRankSpecifier;
            _ArrayRankSpecifierWithExprCreator = ArrayRankSpecifierWithExpr;
            _QueryClauseCreator = QueryClause;
            _OrderingCreator = Ordering;
        }

        protected readonly Creator<ExternAliasDirectiveSyntax> _ExternAliasDirectiveCreator;
        protected readonly Creator<UsingDirectiveSyntax> _UsingDirectiveCreator;
        protected readonly Creator<AttributeListSyntax> _GlobalAttributeListCreator;
        protected readonly Creator<AttributeListSyntax> _AttributeListCreator;
        protected readonly Creator<AttributeSyntax> _AttributeCreator;
        protected readonly Creator<AttributeArgumentSyntax> _AttributeArgumentCreator;
        protected readonly Creator<MemberDeclarationSyntax> _NamespaceMemberCreator;

        protected readonly Creator<SyntaxToken> _ModifierCreator;
        protected readonly Creator<TypeParameterSyntax> _TypeParameterCreator;
        private readonly Creator<BaseTypeSyntax> _BaseTypeCreator;
        private readonly Creator<TypeParameterConstraintSyntax> _TypeParameterConstraintCreator;


        protected readonly Creator<StatementSyntax> _StatementCreator;
        protected readonly Creator<SwitchSectionSyntax> _SwitchSectionCreator;
        protected readonly Creator<SwitchLabelSyntax> _SwitchLabelCreator;
        protected readonly Creator<CatchClauseSyntax> _CatchClauseCreator;
        protected readonly Creator<VariableDeclaratorSyntax> _ConstVariableDeclaratorCreator;
        protected readonly Creator<VariableDeclaratorSyntax> _VariableDeclaratorCreator;

        protected readonly Creator<ExpressionSyntax> _ExpressionCreator;
        protected readonly Creator<ExpressionSyntax> _ArrayInitializerExpressionCreator;

        protected readonly Creator<TypeSyntax> _TypeCreator;
        protected readonly Creator<ArrayRankSpecifierSyntax> _ArrayRankSpecifierCreator;
        protected readonly Creator<ArrayRankSpecifierSyntax> _ArrayRankSpecifierWithExprCreator;

        protected readonly Creator<QueryClauseSyntax> _QueryClauseCreator;
        protected readonly Creator<OrderingSyntax> _OrderingCreator;

        protected static readonly Func<OmittedArraySizeExpressionSyntax> _OmittedArraySizeExpressionCreator = SyntaxFactory.OmittedArraySizeExpression;
        protected static readonly Func<OmittedTypeArgumentSyntax> _OmittedTypeArgumentCreator = SyntaxFactory.OmittedTypeArgument;


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
        protected bool PeekIdentifier(int offset)
        {
            return PeekToken(offset, (int)TokenKind.NormalIdentifier, (int)TokenKind.VerbatimIdentifier);
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
                ++_tokenIndex;
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
        protected SyntaxToken IdentifierOrDefault()
        {
            SyntaxToken result;
            Identifier(out result);
            return result;
        }

        protected bool ReservedKeyword(SyntaxKind kind, out SyntaxToken result)
        {
            var token = GetToken();
            if (token.SyntaxKind == kind)
            {
                ++_tokenIndex;
                result = Token(kind, token.Index);
                return true;
            }
            result = default(SyntaxToken);
            return false;
        }
        protected bool ReservedKeyword(SyntaxKind kind1, SyntaxKind kind2, out SyntaxToken result)
        {
            var token = GetToken();
            var skind = token.SyntaxKind;
            if (skind == kind1 || skind == kind2)
            {
                ++_tokenIndex;
                result = Token(skind, token.Index);
                return true;
            }
            result = default(SyntaxToken);
            return false;
        }
        protected SyntaxToken ReservedKeywordExpected(SyntaxKind kind)
        {
            SyntaxToken result;
            if (!ReservedKeyword(kind, out result))
            {
                ErrorAndThrow(SyntaxFacts.GetText(kind) + " expected.");
            }
            return result;
        }
        protected SyntaxToken ReservedKeywordOrDefault(SyntaxKind kind)
        {
            SyntaxToken result;
            ReservedKeyword(kind, out result);
            return result;
        }

        protected bool ContextualKeyword(string text, out SyntaxToken result)
        {
            var token = GetToken();
            if (token.IsContextualKeyword(text))
            {
                ++_tokenIndex;
                result = Token(SyntaxFacts.GetContextualKeywordKind(text), token.Index);
                return true;
            }
            result = default(SyntaxToken);
            return false;
        }
        protected SyntaxToken ContextualKeywordExpected(string text)
        {
            SyntaxToken result;
            if (!ContextualKeyword(text, out result))
            {
                ErrorAndThrow(text + " expected.");
            }
            return result;
        }
        protected SyntaxToken ContextualKeywordOrDefault(string text)
        {
            SyntaxToken result;
            ContextualKeyword(text, out result);
            return result;
        }

        #region
        protected static SyntaxToken Token(SyntaxKind kind, int tokenIndex)
        {
            return SyntaxFactory.Token(kind).Attach(tokenIndex);
        }
        protected static SyntaxToken Token(int kind, int tokenIndex)
        {
            return Token(_tokenKindMap[kind], tokenIndex);
        }
        private static readonly Dictionary<int, SyntaxKind> _tokenKindMap = new Dictionary<int, SyntaxKind>
        {
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
        protected SyntaxToken TokenOrDefault(int kind)
        {
            SyntaxToken result;
            Token(kind, out result);
            return result;
        }
        #endregion
        #region list
        protected SyntaxList<T> List<T>(Creator<T> itemCreator, string itemExpectedErrorMsg = null) where T : SyntaxNode
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
            if (itemExpectedErrorMsg != null)
            {
                ErrorAndThrow(itemExpectedErrorMsg);
            }
            return default(SyntaxList<T>);
        }
        protected SyntaxTokenList TokenList(Creator<SyntaxToken> itemCreator, string itemExpectedErrorMsg = null)
        {
            SyntaxToken? singleItem = null;
            List<SyntaxToken> itemList = null;
            while (true)
            {
                SyntaxToken item;
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
                            itemList = new List<SyntaxToken>();
                            itemList.Add(singleItem.Value);
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
                return SyntaxFactory.TokenList(itemList);
            }
            if (singleItem != null)
            {
                return SyntaxFactory.TokenList(singleItem.Value);
            }
            if (itemExpectedErrorMsg != null)
            {
                ErrorAndThrow(itemExpectedErrorMsg);
            }
            return default(SyntaxTokenList);
        }
        protected SeparatedSyntaxList<T> SeparatedList<T>(Creator<T> itemCreator, Func<T> omittedItemCreator = null,
            bool allowTrailingSeparator = false, string itemExpectedErrorMsg = null, int separatorKind = ',') where T : SyntaxNode
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
            if (itemExpectedErrorMsg != null)
            {
                ErrorAndThrow(itemExpectedErrorMsg);
            }
            return default(SeparatedSyntaxList<T>);
        }

        #endregion list

        #region

        protected CompilationUnitSyntax CompilationUnit()
        {
            return SyntaxFactory.CompilationUnit(List(_ExternAliasDirectiveCreator), List(_UsingDirectiveCreator), List(_GlobalAttributeListCreator),
                List(_NamespaceMemberCreator));
        }

        protected bool ExternAliasDirective(out ExternAliasDirectiveSyntax result)
        {
            SyntaxToken externKeyword;
            if (ReservedKeyword(SyntaxKind.ExternKeyword, out externKeyword))
            {
                result = SyntaxFactory.ExternAliasDirective(externKeyword, ContextualKeywordExpected("alias"),
                    IdentifierExpected(), TokenExpected(';'));
                return true;
            }
            result = null;
            return false;
        }
        protected bool UsingDirective(out UsingDirectiveSyntax result)
        {
            SyntaxToken usingKeyword;
            if (ReservedKeyword(SyntaxKind.UsingKeyword, out usingKeyword))
            {
                NameEqualsSyntax alias = null;
                var token = GetToken();
                if (token.IsIdentifier)
                {
                    var token2 = GetToken(1);
                    if (token2.Kind == '=')
                    {
                        _tokenIndex += 2;
                        alias = SyntaxFactory.NameEquals(SyntaxFactory.IdentifierName(Identifier(token)), Token(SyntaxKind.EqualsToken, token2.Index));
                    }
                }
                result = SyntaxFactory.UsingDirective(usingKeyword, default(SyntaxToken), alias, NameExpected(), TokenExpected(';'));
                return true;
            }
            result = null;
            return false;
        }
        #region attributes
        protected bool GlobalAttributeList(out AttributeListSyntax result)
        {
            var openBracket = GetToken();
            if (openBracket.Kind == '[')
            {
                SyntaxKind targetKind = SyntaxKind.None;
                var target = GetToken(1);
                if (target.IsNormalIdentifier)
                {
                    switch (target.Value)
                    {
                        case "assembly":
                            targetKind = SyntaxKind.AssemblyKeyword;
                            break;
                        case "module":
                            targetKind = SyntaxKind.ModuleKeyword;
                            break;
                    }
                    if (targetKind != SyntaxKind.None)
                    {
                        var colon = GetToken(2);
                        if (colon.Kind == ':')
                        {
                            _tokenIndex += 3;
                            result = SyntaxFactory.AttributeList(Token(SyntaxKind.OpenBracketToken, openBracket.Index),
                                SyntaxFactory.AttributeTargetSpecifier(Token(targetKind, target.Index), Token(SyntaxKind.ColonToken, colon.Index)),
                                SeparatedList(_AttributeCreator, null, true, "Attribute expected."), TokenExpected(']'));
                            return true;
                        }
                    }
                }
            }
            result = null;
            return false;
        }

        protected bool AttributeList(out AttributeListSyntax result)
        {
            var openBracket = GetToken();
            if (openBracket.Kind == '[')
            {
                SyntaxKind targetKind = SyntaxKind.None;
                var target = GetToken(1);
                switch (target.Kind)
                {
                    case (int)TokenKind.ReservedKeyword:
                        switch (target.SyntaxKind)
                        {
                            case SyntaxKind.EventKeyword:
                            case SyntaxKind.ReturnKeyword:
                                targetKind = target.SyntaxKind;
                                break;
                        }
                        break;
                    case (int)TokenKind.NormalIdentifier:
                        switch (target.Value)
                        {
                            case "field":
                                targetKind = SyntaxKind.FieldKeyword;
                                break;
                            case "method":
                                targetKind = SyntaxKind.MethodKeyword;
                                break;
                            case "param":
                                targetKind = SyntaxKind.ParamKeyword;
                                break;
                            case "property":
                                targetKind = SyntaxKind.PropertyKeyword;
                                break;
                            case "type":
                                targetKind = SyntaxKind.TypeKeyword;
                                break;
                            case "typevar":
                                targetKind = SyntaxKind.TypeVarKeyword;
                                break;
                        }
                        break;
                }
                var colon = default(SyntaxToken);
                if (targetKind != SyntaxKind.None)
                {
                    var colonToken = GetToken(2);
                    if (colonToken.Kind == ':')
                    {
                        _tokenIndex += 2;
                        colon = Token(SyntaxKind.ColonToken, colonToken.Index);
                    }
                    else
                    {
                        targetKind = SyntaxKind.None;
                    }
                }
                ++_tokenIndex;
                result = SyntaxFactory.AttributeList(Token(SyntaxKind.OpenBracketToken, openBracket.Index),
                    targetKind == SyntaxKind.None ? null : SyntaxFactory.AttributeTargetSpecifier(Token(targetKind, target.Index), colon),
                    SeparatedList(_AttributeCreator, null, true, "Attribute expected."), TokenExpected(']'));
                return true;
            }
            result = null;
            return false;
        }
        private bool Attribute(out AttributeSyntax result)
        {
            NameSyntax name;
            if (Name(out name))
            {
                AttributeArgumentListSyntax argList = null;
                var openParen = GetToken();
                if (openParen.Kind == '(')
                {
                    ++_tokenIndex;
                    argList = SyntaxFactory.AttributeArgumentList(Token(SyntaxKind.OpenParenToken, openParen.Index),
                        SeparatedList(_AttributeArgumentCreator), TokenExpected(')'));
                }
                result = SyntaxFactory.Attribute(name, argList);
                return true;
            }
            result = null;
            return false;
        }
        private bool AttributeArgument(out AttributeArgumentSyntax result)
        {
            NameEqualsSyntax nameEquals = null;
            NameColonSyntax nameColon = null;
            var token = GetToken();
            if (token.IsIdentifier)
            {
                var token2 = GetToken(1);
                switch (token2.Kind)
                {
                    case '=':
                        _tokenIndex += 2;
                        nameEquals = SyntaxFactory.NameEquals(SyntaxFactory.IdentifierName(Identifier(token)), Token(SyntaxKind.EqualsToken, token2.Index));
                        break;
                    case ':':
                        _tokenIndex += 2;
                        nameColon = SyntaxFactory.NameColon(SyntaxFactory.IdentifierName(Identifier(token)), Token(SyntaxKind.ColonToken, token2.Index));
                        break;
                }
            }
            ExpressionSyntax expr;
            if (Expression(out expr))
            {
                result = SyntaxFactory.AttributeArgument(nameEquals, nameColon, expr);
                return true;
            }
            result = null;
            return false;
        }
        #endregion attributes

        protected bool NamespaceMember(out MemberDeclarationSyntax result)
        {
            var token = GetToken();
            if (token.SyntaxKind == SyntaxKind.NamespaceKeyword)
            {
                ++_tokenIndex;
                result = SyntaxFactory.NamespaceDeclaration(Token(SyntaxKind.NamespaceKeyword, token.Index), NamespaceNameExpeced(), TokenExpected('{'),
                    List(_ExternAliasDirectiveCreator), List(_UsingDirectiveCreator), List(_NamespaceMemberCreator), TokenExpected('}'), TokenOrDefault(';'));
                return true;
            }
            var attributeLists = List(_AttributeListCreator);
            var modifier = TokenList(_ModifierCreator);
            token = GetToken();
            switch (token.Kind)
            {
                case (int)TokenKind.ReservedKeyword:
                    switch (token.SyntaxKind)
                    {
                        case SyntaxKind.ClassKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.ClassDeclaration();
                            return true;
                        case SyntaxKind.StructKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.StructDeclaration();
                            return true;
                        case SyntaxKind.InterfaceKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.InterfaceDeclaration();
                            return true;
                        case SyntaxKind.EnumKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.EnumDeclaration();
                            return true;
                        case SyntaxKind.DelegateKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.DelegateDeclaration();
                            return true;
                    }
                    break;
                    //case (int)TokenKind.NormalIdentifier:
                    //    break;

            }


            result = null;
            return false;
        }
        protected bool NamespaceName(out NameSyntax result)
        {
            IdentifierNameSyntax ins;
            if (IdentifierName(out ins))
            {
                result = ins;
                SyntaxToken dot;
                while (Token('.', out dot))
                {
                    result = SyntaxFactory.QualifiedName(result, dot, IdentifierNameExpected());
                }
                return true;
            }
            result = null;
            return false;
        }
        protected NameSyntax NamespaceNameExpeced()
        {
            NameSyntax result;
            if (!NamespaceName(out result))
            {
                ErrorAndThrow("Namespace name expected.");
            }
            return result;
        }

        private bool Modifier(out SyntaxToken result)
        {
            var token = GetToken();
            switch (token.Kind)
            {
                case (int)TokenKind.ReservedKeyword:
                    switch (token.SyntaxKind)
                    {
                        case SyntaxKind.PublicKeyword:
                        case SyntaxKind.ProtectedKeyword:
                        case SyntaxKind.InternalKeyword:
                        case SyntaxKind.PrivateKeyword:
                        case SyntaxKind.AbstractKeyword:
                        case SyntaxKind.VirtualKeyword:
                        case SyntaxKind.OverrideKeyword:
                        case SyntaxKind.SealedKeyword:
                        case SyntaxKind.StaticKeyword:
                        case SyntaxKind.NewKeyword:
                        case SyntaxKind.ReadOnlyKeyword:
                        case SyntaxKind.ConstKeyword:
                        case SyntaxKind.VolatileKeyword:
                        case SyntaxKind.ExternKeyword:
                            ++_tokenIndex;
                            result = Token(token.SyntaxKind, token.Index);
                            return true;
                    }
                    break;
                    //case (int)TokenKind.NormalIdentifier:
                    //    switch (token.Value)
                    //    {
                    //        case "partial":
                    //            ++_tokenIndex;
                    //            result = Token(SyntaxKind.PartialKeyword, token.Index);
                    //            return true;
                    //        case "async":
                    //            ++_tokenIndex;
                    //            result = Token(SyntaxKind.AsyncKeyword, token.Index);
                    //            return true;
                    //    }
                    //    break;
            }
            result = default(SyntaxToken);
            return false;
        }

        protected bool TypeParameterList(out TypeParameterListSyntax result)
        {
            SyntaxToken lessThan;
            if (Token('<', out lessThan))
            {
                result = SyntaxFactory.TypeParameterList(lessThan, SeparatedList(_TypeParameterCreator, null, false, "Type parameter expected."),
                    TokenExpected('>'));
                return true;
            }
            result = null;
            return false;
        }
        private bool TypeParameter(out TypeParameterSyntax result)
        {
            var attributeLists = List(_AttributeListCreator);
            SyntaxToken varianceKeyword;
            ReservedKeyword(SyntaxKind.InKeyword, SyntaxKind.OutKeyword, out varianceKeyword);
            SyntaxToken identifier;
            if (Identifier(out identifier))
            {
                result = SyntaxFactory.TypeParameter(attributeLists, varianceKeyword, identifier);
                return true;
            }
            if (attributeLists.Count > 0 || varianceKeyword.RawKind > 0)
            {
                IdentifierExpectedErrorAndThrow();
            }
            result = null;
            return false;
        }

        protected bool BaseList(out BaseListSyntax result)
        {
            SyntaxToken colon;
            if (Token(':', out colon))
            {
                result = SyntaxFactory.BaseList(colon, SeparatedList(_BaseTypeCreator, null, false, "Type expected."));
                return true;
            }
            result = null;
            return false;
        }
        private bool BaseType(out BaseTypeSyntax result)
        {
            TypeSyntax type;
            NameSyntax name;
            PredefinedTypeSyntax pt;
            if (Name(out name))
            {
                type = name;
            }
            else if (PredefinedType(out pt))
            {
                type = pt;
            }
            else
            {
                result = null;
                return false;
            }
            result = SyntaxFactory.SimpleBaseType(type);
            return true;
        }
        private bool TypeParameterConstraintClause(out TypeParameterConstraintClauseSyntax result)
        {
            SyntaxToken where;
            if (ContextualKeyword("where", out where))
            {
                result = SyntaxFactory.TypeParameterConstraintClause(where, IdentifierNameExpected(), TokenExpected(':'),
                    SeparatedList(_TypeParameterConstraintCreator, null, false, "Type parameter constraint expected."));
                return true;
            }
            result = null;
            return false;
        }
        private bool TypeParameterConstraint(out TypeParameterConstraintSyntax result)
        {
            var token = GetToken();
            switch (token.SyntaxKind)
            {
                case SyntaxKind.NewKeyword:
                    ++_tokenIndex;
                    result = SyntaxFactory.ConstructorConstraint(Token(SyntaxKind.NewKeyword, token.Index), TokenExpected('('), TokenExpected(')'));
                    return true;
                case SyntaxKind.ClassKeyword:
                    ++_tokenIndex;
                    result = SyntaxFactory.ClassOrStructConstraint(SyntaxKind.ClassConstraint, Token(SyntaxKind.ClassKeyword, token.Index));
                    return true;
                case SyntaxKind.StructKeyword:
                    ++_tokenIndex;
                    result = SyntaxFactory.ClassOrStructConstraint(SyntaxKind.StructConstraint, Token(SyntaxKind.StructKeyword, token.Index));
                    return true;
            }
            TypeSyntax type;
            if (Type(out type))
            {
                result = SyntaxFactory.TypeConstraint(type);
                return true;
            }
            result = null;
            return false;
        }
        protected bool ClassOrStructMember(out MemberDeclarationSyntax result)
        {


            result = null;
            return false;
        }

        private bool ExplicitInterfaceSpecifier(out ExplicitInterfaceSpecifierSyntax result)
        {
            if (PeekIdentifier(0) &&
                PeekToken(1, '.', '<', (int)TokenKind.ColonColon))
            {
                NameSyntax name;
                NameStart(out name);
                SyntaxToken dot;
                while (Token('.', out dot))
                {
                    if (PeekIdentifier(0) && !PeekToken(1, '.'))
                    {
                        result = SyntaxFactory.ExplicitInterfaceSpecifier(name, dot);
                        return true;
                    }
                    name = SyntaxFactory.QualifiedName(name, dot, SimpleNameExpected());
                }
            }
            result = null;
            return false;
        }

        #endregion

        #region statements
        protected StatementSyntax StatementExpected(bool embeddedOnly = false)
        {
            StatementSyntax result;
            if (!StatementCore(out result, embeddedOnly))
            {
                StatementExpectedErrorAndThrow();
            }
            return result;
        }
        protected void StatementExpectedErrorAndThrow()
        {
            ErrorAndThrow("Statement expected.");
        }
        protected bool Statement(out StatementSyntax result)
        {
            return StatementCore(out result);
        }
        protected bool StatementCore(out StatementSyntax result, bool embeddedOnly = false)
        {
            var token = GetToken();
            switch (token.Kind)
            {
                case (int)TokenKind.ReservedKeyword:
                    switch (token.SyntaxKind)
                    {
                        case SyntaxKind.IfKeyword:
                            {
                                ++_tokenIndex;
                                var openParen = TokenExpected('(');
                                var condition = ExpressionExpected();
                                var closeParen = TokenExpected(')');
                                var stm = StatementExpected(true);
                                ElseClauseSyntax @else = null;
                                SyntaxToken elseKeyword;
                                if (ReservedKeyword(SyntaxKind.ElseKeyword, out elseKeyword))
                                {
                                    @else = SyntaxFactory.ElseClause(elseKeyword, StatementExpected(true));
                                }
                                result = SyntaxFactory.IfStatement(Token(SyntaxKind.IfKeyword, token.Index), openParen, condition, closeParen,
                                    stm, @else);
                                return true;
                            }
                        case SyntaxKind.SwitchKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.SwitchStatement(Token(SyntaxKind.SwitchKeyword, token.Index), TokenExpected('('), ExpressionExpected(), TokenExpected(')'),
                                TokenExpected('{'), List(_SwitchSectionCreator), TokenExpected('}'));
                            return true;
                        case SyntaxKind.WhileKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.WhileStatement(Token(SyntaxKind.WhileKeyword, token.Index), TokenExpected('('), ExpressionExpected(),
                                TokenExpected(')'), StatementExpected(true));
                            return true;
                        case SyntaxKind.DoKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.DoStatement(Token(SyntaxKind.DoKeyword, token.Index), StatementExpected(true),
                                ReservedKeywordExpected(SyntaxKind.WhileKeyword), TokenExpected('('), ExpressionExpected(), TokenExpected(')'),
                                TokenExpected(';'));
                            return true;
                        case SyntaxKind.ForEachKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.ForEachStatement(Token(SyntaxKind.ForEachKeyword, token.Index), TokenExpected('('), TypeExpected(),
                                IdentifierExpected(), ReservedKeywordExpected(SyntaxKind.InKeyword), ExpressionExpected(), TokenExpected(')'), StatementExpected(true));
                            return true;
                        case SyntaxKind.ForKeyword:
                            {
                                ++_tokenIndex;
                                var openParen = TokenExpected('(');
                                SeparatedSyntaxList<ExpressionSyntax> initializers = default(SeparatedSyntaxList<ExpressionSyntax>);
                                var declaration = VariableDeclarationOrLabeledStatement() as VariableDeclarationSyntax;
                                if (declaration == null)
                                {
                                    initializers = SeparatedList(_ExpressionCreator);
                                }
                                var firstSemicolon = TokenExpected(';');
                                ExpressionSyntax condition;
                                Expression(out condition);
                                result = SyntaxFactory.ForStatement(Token(SyntaxKind.ForKeyword, token.Index), openParen, declaration, initializers,
                                    firstSemicolon, condition, TokenExpected(';'), SeparatedList(_ExpressionCreator), TokenExpected(')'), StatementExpected(true));
                                return true;
                            }
                        case SyntaxKind.BreakKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.BreakStatement(Token(SyntaxKind.BreakKeyword, token.Index), TokenExpected(';'));
                            return true;
                        case SyntaxKind.ContinueKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.ContinueStatement(Token(SyntaxKind.ContinueKeyword, token.Index), TokenExpected(';'));
                            return true;
                        case SyntaxKind.GotoKeyword:
                            {
                                ++_tokenIndex;
                                SyntaxKind kind;
                                SyntaxToken caseOrDefaultKeyword;
                                ExpressionSyntax expr;
                                IdentifierNameSyntax ins;
                                if (IdentifierName(out ins))
                                {
                                    expr = ins;
                                    kind = SyntaxKind.GotoStatement;
                                    caseOrDefaultKeyword = default(SyntaxToken);
                                }
                                else
                                {
                                    var token2 = GetToken();
                                    switch (token2.SyntaxKind)
                                    {
                                        case SyntaxKind.CaseKeyword:
                                            ++_tokenIndex;
                                            expr = ExpressionExpected();
                                            kind = SyntaxKind.GotoCaseStatement;
                                            caseOrDefaultKeyword = Token(SyntaxKind.CaseKeyword, token2.Index);
                                            break;
                                        case SyntaxKind.DefaultKeyword:
                                            ++_tokenIndex;
                                            kind = SyntaxKind.GotoDefaultStatement;
                                            caseOrDefaultKeyword = Token(SyntaxKind.DefaultKeyword, token2.Index);
                                            expr = null;
                                            break;
                                        default:
                                            IdentifierExpectedErrorAndThrow();
                                            throw new Exception();
                                    }
                                }
                                result = SyntaxFactory.GotoStatement(kind, Token(SyntaxKind.GotoKeyword, token.Index), caseOrDefaultKeyword, expr, TokenExpected(';'));
                                return true;
                            }
                        case SyntaxKind.ReturnKeyword:
                            {
                                ++_tokenIndex;
                                SyntaxToken semicolon;
                                ExpressionSyntax expr = null;
                                if (!Token(';', out semicolon))
                                {
                                    expr = ExpressionExpected();
                                    semicolon = TokenExpected(';');
                                }
                                result = SyntaxFactory.ReturnStatement(Token(SyntaxKind.ReturnKeyword, token.Index), expr, semicolon);
                                return true;
                            }
                        case SyntaxKind.ThrowKeyword:
                            {
                                ++_tokenIndex;
                                SyntaxToken semicolon;
                                ExpressionSyntax expr = null;
                                if (!Token(';', out semicolon))
                                {
                                    expr = ExpressionExpected();
                                    semicolon = TokenExpected(';');
                                }
                                result = SyntaxFactory.ThrowStatement(Token(SyntaxKind.ThrowKeyword, token.Index), expr, semicolon);
                                return true;
                            }
                        case SyntaxKind.TryKeyword:
                            {
                                ++_tokenIndex;
                                var block = BlockExpected();
                                var catches = List(_CatchClauseCreator);
                                BlockSyntax finallyBlock = null;
                                var token2 = GetToken();
                                if (token2.SyntaxKind == SyntaxKind.FinallyKeyword)
                                {
                                    ++_tokenIndex;
                                    finallyBlock = BlockExpected();
                                }
                                result = SyntaxFactory.TryStatement(Token(SyntaxKind.TryKeyword, token.Index), block, catches,
                                    finallyBlock == null ? null : SyntaxFactory.FinallyClause(Token(SyntaxKind.FinallyKeyword, token2.Index), finallyBlock));
                                return true;
                            }
                        case SyntaxKind.CheckedKeyword:
                        case SyntaxKind.UncheckedKeyword:
                            if (PeekToken(1, '{'))
                            {
                                ++_tokenIndex;
                                var isChecked = token.SyntaxKind == SyntaxKind.CheckedKeyword;
                                result = SyntaxFactory.CheckedStatement(isChecked ? SyntaxKind.CheckedStatement : SyntaxKind.UncheckedStatement,
                                    Token(isChecked ? SyntaxKind.CheckedKeyword : SyntaxKind.UncheckedKeyword, token.Index), BlockExpected());
                                return true;
                            }
                            break;
                        case SyntaxKind.LockKeyword:
                            ++_tokenIndex;
                            result = SyntaxFactory.LockStatement(Token(SyntaxKind.LockKeyword, token.Index), TokenExpected('('), ExpressionExpected(), TokenExpected(')'),
                                StatementExpected(true));
                            return true;
                        case SyntaxKind.UsingKeyword:
                            {
                                ++_tokenIndex;
                                var openParen = TokenExpected('(');
                                ExpressionSyntax expr = null;
                                var declaration = VariableDeclarationOrLabeledStatement() as VariableDeclarationSyntax;
                                if (declaration == null)
                                {
                                    expr = ExpressionExpected();
                                }
                                result = SyntaxFactory.UsingStatement(Token(SyntaxKind.UsingKeyword, token.Index), openParen, declaration, expr,
                                    TokenExpected(')'), StatementExpected(true));
                                return true;
                            }
                        case SyntaxKind.ConstKeyword:
                            if (embeddedOnly)
                            {
                                result = null;
                                return false;
                            }
                            else
                            {
                                ++_tokenIndex;
                                result = SyntaxFactory.LocalDeclarationStatement(SyntaxFactory.TokenList(Token(SyntaxKind.ConstKeyword, token.Index)),
                                    SyntaxFactory.VariableDeclaration(TypeExpected(), SeparatedList(_ConstVariableDeclaratorCreator, null, false, "Variable declarator expected.")),
                                    TokenExpected(';'));
                                return true;
                            }
                    }
                    break;
                case (int)TokenKind.NormalIdentifier:
                    switch (token.Value)
                    {
                        case "yield":
                            {
                                var token2 = GetToken(1);
                                switch (token2.SyntaxKind)
                                {
                                    case SyntaxKind.BreakKeyword:
                                        _tokenIndex += 2;
                                        result = SyntaxFactory.YieldStatement(SyntaxKind.YieldBreakStatement, Token(SyntaxKind.YieldKeyword, token.Index),
                                            Token(SyntaxKind.BreakKeyword, token.Index), null, TokenExpected(';'));
                                        return true;
                                    case SyntaxKind.ReturnKeyword:
                                        _tokenIndex += 2;
                                        result = SyntaxFactory.YieldStatement(SyntaxKind.YieldReturnStatement, Token(SyntaxKind.YieldKeyword, token.Index),
                                            Token(SyntaxKind.ReturnKeyword, token.Index), ExpressionExpected(), TokenExpected(';'));
                                        return true;
                                }
                            }
                            break;
                    }
                    break;
                case ';':
                    ++_tokenIndex;
                    result = SyntaxFactory.EmptyStatement(Token(SyntaxKind.SemicolonToken, token.Index));
                    return true;
                case '{':
                    ++_tokenIndex;
                    result = SyntaxFactory.Block(Token(SyntaxKind.OpenBraceToken, token.Index), List(_StatementCreator), TokenExpected('}'));
                    return true;

            }
            if (!embeddedOnly)
            {
                var sn = VariableDeclarationOrLabeledStatement(true);
                if (sn != null)
                {
                    var variableDeclaration = sn as VariableDeclarationSyntax;
                    if (variableDeclaration != null)
                    {
                        result = SyntaxFactory.LocalDeclarationStatement(default(SyntaxTokenList), variableDeclaration, TokenExpected(';'));
                    }
                    else
                    {
                        result = (StatementSyntax)sn;
                    }
                    return true;
                }
            }
            ExpressionSyntax stmExpr;
            if (Expression(out stmExpr))
            {
                result = SyntaxFactory.ExpressionStatement(stmExpr, TokenExpected(';'));
                return true;
            }
            result = null;
            return false;
        }
        protected BlockSyntax BlockExpected()
        {
            BlockSyntax result;
            if (!Block(out result))
            {
                ErrorAndThrow("Block statement expected.");
            }
            return result;
        }
        protected bool Block(out BlockSyntax result)
        {
            SyntaxToken st;
            if (Token('{', out st))
            {
                result = SyntaxFactory.Block(st, List(_StatementCreator), TokenExpected('}'));
                return true;
            }
            result = null;
            return false;
        }
        private SyntaxNode VariableDeclarationOrLabeledStatement(bool allowLabeledStm = false)
        {
            var token = GetToken();
            var isId = token.IsIdentifier;
            if (isId || token.IsPredefinedType)
            {
                TypeSyntax type = null;
                var token2 = GetToken(1);
                switch (token2.Kind)
                {
                    case ':':
                        if (allowLabeledStm && isId)
                        {
                            _tokenIndex += 2;
                            return SyntaxFactory.LabeledStatement(Identifier(token), Token(SyntaxKind.ColonToken, token2.Index), StatementExpected());
                        }
                        return null;
                    case (int)TokenKind.NormalIdentifier:
                    case (int)TokenKind.VerbatimIdentifier:
                        Type(out type);
                        break;
                    case '.':
                    case '<':
                    case (int)TokenKind.ColonColon:
                    case '?':
                    case '[':
                        {
                            var tkIdx = _tokenIndex;
                            Type(out type);
                            if (!PeekToken(0, (int)TokenKind.NormalIdentifier, (int)TokenKind.VerbatimIdentifier))
                            {
                                type = null;
                                _tokenIndex = tkIdx;
                            }
                        }
                        break;
                }
                if (type != null)
                {
                    return SyntaxFactory.VariableDeclaration(type, SeparatedList(_VariableDeclaratorCreator, null, false, "Variable declarator expected."));
                }
            }
            return null;
        }
        private bool ConstVariableDeclarator(out VariableDeclaratorSyntax result)
        {
            SyntaxToken id;
            if (Identifier(out id))
            {
                result = SyntaxFactory.VariableDeclarator(id, null, SyntaxFactory.EqualsValueClause(TokenExpected('='), ExpressionExpected()));
                return true;
            }
            result = null;
            return false;
        }
        private bool VariableDeclarator(out VariableDeclaratorSyntax result)
        {
            SyntaxToken id;
            if (Identifier(out id))
            {
                var equals = TokenExpected('=');
                ExpressionSyntax expr;
                if (PeekToken(0, '{'))
                {
                    expr = ArrayInitializer();
                }
                else
                {
                    expr = ExpressionExpected();
                }
                result = SyntaxFactory.VariableDeclarator(id, null, SyntaxFactory.EqualsValueClause(equals, expr));
                return true;
            }
            result = null;
            return false;
        }
        private LabeledStatementSyntax LabeledStatement(Token id)
        {
            var token2 = GetToken(1);
            if (token2.Kind == ':')
            {
                _tokenIndex += 2;
                return SyntaxFactory.LabeledStatement(Identifier(id), Token(SyntaxKind.ColonToken, token2.Index), StatementExpected());
            }
            return null;
        }
        private bool CatchClause(out CatchClauseSyntax result)
        {
            var token = GetToken();
            if (token.SyntaxKind == SyntaxKind.CatchKeyword)
            {
                ++_tokenIndex;
                var openParen = TokenExpected('(');
                var type = TypeExpected();
                SyntaxToken id;
                Identifier(out id);
                result = SyntaxFactory.CatchClause(Token(SyntaxKind.CatchKeyword, token.Index),
                    SyntaxFactory.CatchDeclaration(openParen, type, id, TokenExpected(')')), null,
                    BlockExpected());
                return true;
            }
            result = null;
            return false;
        }

        private bool SwitchSection(out SwitchSectionSyntax result)
        {
            var labels = List(_SwitchLabelCreator);
            if (labels.Count > 0)
            {
                result = SyntaxFactory.SwitchSection(labels, List(_StatementCreator, "Statement expected."));
                return true;
            }
            result = null;
            return false;
        }
        private bool SwitchLabel(out SwitchLabelSyntax result)
        {
            var token = GetToken();
            switch (token.SyntaxKind)
            {
                case SyntaxKind.CaseKeyword:
                    ++_tokenIndex;
                    result = SyntaxFactory.CaseSwitchLabel(Token(SyntaxKind.CaseKeyword, token.Index), ExpressionExpected(), TokenExpected(':'));
                    return true;
                case SyntaxKind.DefaultKeyword:
                    ++_tokenIndex;
                    result = SyntaxFactory.DefaultSwitchLabel(Token(SyntaxKind.DefaultKeyword, token.Index), TokenExpected(':'));
                    return true;
            }
            result = null;
            return false;
        }

        #endregion statements
        #region expressions
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
                                if (PeekToken(1, (int)TokenKind.NormalIdentifier, (int)TokenKind.VerbatimIdentifier, (int)TokenKind.ReservedKeyword))
                                {
                                    ++_tokenIndex;
                                    var tkIdx = _tokenIndex;
                                    //>from id in
                                    SyntaxToken identifier;
                                    if (Identifier(out identifier))
                                    {
                                        SyntaxToken inKeyword;
                                        if (ReservedKeyword(SyntaxKind.InKeyword, out inKeyword))
                                        {
                                            result = QueryExpression(token.Index, null, identifier, inKeyword);
                                            return true;
                                        }
                                    }
                                    _tokenIndex = tkIdx;
                                    //>from type id in
                                    result = QueryExpression(token.Index, TypeExpected(), IdentifierExpected(), ReservedKeywordExpected(SyntaxKind.InKeyword));
                                    return true;
                                }
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
                                            result = SimpleLambdaExpression(token.Index, token2);
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
                                            result = ParenthesizedLambdaExpression(token.Index, token2.Index);
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
                                result = SimpleLambdaExpression(null, token);
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
                        result = SimpleLambdaExpression(null, token);
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
                        result = ParenthesizedLambdaExpression(null, token.Index);
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
                    SyntaxKind assignExprKind = SyntaxKind.None;
                    token = GetToken();
                    switch (token.Kind)
                    {
                        case '=':
                            assignExprKind = SyntaxKind.SimpleAssignmentExpression;
                            break;
                        case (int)TokenKind.PlusEquals:
                            assignExprKind = SyntaxKind.AddAssignmentExpression;
                            break;
                        case (int)TokenKind.MinusEquals:
                            assignExprKind = SyntaxKind.SubtractAssignmentExpression;
                            break;
                        case (int)TokenKind.AsteriskEquals:
                            assignExprKind = SyntaxKind.MultiplyAssignmentExpression;
                            break;
                        case (int)TokenKind.SlashEquals:
                            assignExprKind = SyntaxKind.DivideAssignmentExpression;
                            break;
                        case (int)TokenKind.PercentEquals:
                            assignExprKind = SyntaxKind.ModuloAssignmentExpression;
                            break;
                        case (int)TokenKind.AmpersandEquals:
                            assignExprKind = SyntaxKind.AndAssignmentExpression;
                            break;
                        case (int)TokenKind.BarEquals:
                            assignExprKind = SyntaxKind.OrAssignmentExpression;
                            break;
                        case (int)TokenKind.CaretEquals:
                            assignExprKind = SyntaxKind.ExclusiveOrAssignmentExpression;
                            break;
                        case (int)TokenKind.LessThanLessThanEquals:
                            assignExprKind = SyntaxKind.LeftShiftAssignmentExpression;
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
                    if (assignExprKind != SyntaxKind.None)
                    {
                        ++_tokenIndex;
                        result = SyntaxFactory.AssignmentExpression(assignExprKind, result, Token(token.Kind, token.Index), ExpressionExpected());
                    }
                }
                return true;
            }
            return false;
        }
        #region query expr
        private QueryExpressionSyntax QueryExpression(int fromTokenIndex, TypeSyntax type, SyntaxToken identifier, SyntaxToken inKeyword)
        {
            return SyntaxFactory.QueryExpression(
                SyntaxFactory.FromClause(Token(SyntaxKind.FromKeyword, fromTokenIndex), type, identifier, inKeyword, ExpressionExpected()),
                QueryBodyExpected());
        }
        private QueryBodySyntax QueryBodyExpected()
        {
            QueryBodySyntax result;
            if (!QueryBody(out result))
            {
                ErrorAndThrow("Query body expected.");
            }
            return result;
        }
        private bool QueryBody(out QueryBodySyntax result)
        {
            var clauses = List(_QueryClauseCreator);
            SelectOrGroupClauseSyntax selectOrGroup = null;
            var token = GetToken();
            if (token.IsNormalIdentifier)
            {
                switch (token.Value)
                {
                    case "select":
                        ++_tokenIndex;
                        selectOrGroup = SyntaxFactory.SelectClause(Token(SyntaxKind.SelectKeyword, token.Index), ExpressionExpected());
                        break;
                    case "group":
                        ++_tokenIndex;
                        selectOrGroup = SyntaxFactory.GroupClause(Token(SyntaxKind.GroupKeyword, token.Index), ExpressionExpected(),
                            ContextualKeywordExpected("by"), ExpressionExpected());
                        break;
                }
            }
            if (selectOrGroup != null)
            {
                QueryContinuationSyntax continuation = null;
                SyntaxToken intoKeyword;
                if (ContextualKeyword("into", out intoKeyword))
                {
                    continuation = SyntaxFactory.QueryContinuation(intoKeyword, IdentifierExpected(), QueryBodyExpected());
                }
                result = SyntaxFactory.QueryBody(clauses, selectOrGroup, continuation);
                return true;
            }
            result = null;
            return false;
        }
        private bool QueryClause(out QueryClauseSyntax result)
        {
            var token = GetToken();
            if (token.IsNormalIdentifier)
            {
                switch (token.Value)
                {
                    case "from":
                        {
                            TypeSyntax type = null;
                            SyntaxToken identifier;
                            SyntaxToken inKeyword = default(SyntaxToken);
                            var ok = false;
                            ++_tokenIndex;
                            var tkIdx = _tokenIndex;
                            //>from id in
                            if (Identifier(out identifier))
                            {
                                if (ReservedKeyword(SyntaxKind.InKeyword, out inKeyword))
                                {
                                    ok = true;
                                }
                            }
                            if (!ok)
                            {
                                _tokenIndex = tkIdx;
                                //>from type id in
                                type = TypeExpected();
                                identifier = IdentifierExpected();
                                inKeyword = ReservedKeywordExpected(SyntaxKind.InKeyword);
                            }
                            result = SyntaxFactory.FromClause(Token(SyntaxKind.FromKeyword, token.Index), type, identifier,
                                inKeyword, ExpressionExpected());
                            return true;
                        }
                    case "let":
                        ++_tokenIndex;
                        result = SyntaxFactory.LetClause(Token(SyntaxKind.LetKeyword, token.Index), IdentifierExpected(), TokenExpected('='), ExpressionExpected());
                        return true;
                    case "join":
                        {
                            TypeSyntax type = null;
                            SyntaxToken identifier;
                            SyntaxToken inKeyword = default(SyntaxToken);
                            var ok = false;
                            ++_tokenIndex;
                            var tkIdx = _tokenIndex;
                            //>join id in
                            if (Identifier(out identifier))
                            {
                                if (ReservedKeyword(SyntaxKind.InKeyword, out inKeyword))
                                {
                                    ok = true;
                                }
                            }
                            if (!ok)
                            {
                                _tokenIndex = tkIdx;
                                //>join type id in
                                type = TypeExpected();
                                identifier = IdentifierExpected();
                                inKeyword = ReservedKeywordExpected(SyntaxKind.InKeyword);
                            }
                            var inExpr = ExpressionExpected();
                            var onKeyword = ContextualKeywordExpected("on");
                            var leftExpr = ExpressionExpected();
                            var equalsKeyword = ContextualKeywordExpected("equals");
                            var rightExpr = ExpressionExpected();
                            JoinIntoClauseSyntax intoClause = null;
                            SyntaxToken intoKeyword;
                            if (ContextualKeyword("into", out intoKeyword))
                            {
                                intoClause = SyntaxFactory.JoinIntoClause(intoKeyword, IdentifierExpected());
                            }
                            result = SyntaxFactory.JoinClause(Token(SyntaxKind.JoinKeyword, token.Index), type, identifier,
                                inKeyword, inExpr, onKeyword, leftExpr, equalsKeyword, rightExpr, intoClause);
                            return true;
                        }
                    case "where":
                        ++_tokenIndex;
                        result = SyntaxFactory.WhereClause(Token(SyntaxKind.WhereKeyword, token.Index), ExpressionExpected());
                        return true;
                    case "orderby":
                        {
                            ++_tokenIndex;
                            result = SyntaxFactory.OrderByClause(Token(SyntaxKind.OrderByKeyword, token.Index),
                                SeparatedList(_OrderingCreator, null, false, "Ordering expected."));
                            return true;
                        }
                }
            }
            result = null;
            return false;
        }
        private bool Ordering(out OrderingSyntax result)
        {
            ExpressionSyntax expr;
            if (Expression(out expr))
            {
                var kind = SyntaxKind.AscendingOrdering;
                SyntaxToken ascendingOrDescendingKeyword;
                if (ContextualKeyword("descending", out ascendingOrDescendingKeyword))
                {
                    kind = SyntaxKind.DescendingOrdering;
                }
                else
                {
                    ContextualKeyword("ascending", out ascendingOrDescendingKeyword);
                }
                result = SyntaxFactory.Ordering(kind, expr, ascendingOrDescendingKeyword);
                return true;
            }
            result = null;
            return false;
        }

        #endregion query expr
        #region lambda expr
        private SimpleLambdaExpressionSyntax SimpleLambdaExpression(int? asyncTokenIndex, Token idToken)
        {
            SyntaxToken arrowToken;
            if (Token((int)TokenKind.EqualsGreaterThan, out arrowToken))
            {
                return SyntaxFactory.SimpleLambdaExpression(asyncTokenIndex == null ? default(SyntaxToken) : Token(SyntaxKind.AsyncKeyword, asyncTokenIndex.Value),
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
        private ParenthesizedLambdaExpressionSyntax ParenthesizedLambdaExpression(int? asyncTokenIndex, int openParenTokenIndex)
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
                                case '.':
                                case '<':
                                case (int)TokenKind.ColonColon:
                                case '?':
                                case '[':
                                case (int)TokenKind.NormalIdentifier:
                                case (int)TokenKind.VerbatimIdentifier:
                                    {
                                        TypeSyntax type;
                                        TypeCore(out type, false, false, false);
                                        SyntaxToken id;
                                        if (!Identifier(out id))
                                        {
                                            if (paraList == null)
                                            {
                                                return null;
                                            }
                                            IdentifierExpectedErrorAndThrow();
                                        }
                                        token = GetToken();
                                        switch (token.Kind)
                                        {
                                            case ',':
                                                ++_tokenIndex;
                                                Extensions.CreateAndAdd(ref paraList, SyntaxFactory.Parameter(default(SyntaxList<AttributeListSyntax>),
                                                     default(SyntaxTokenList), type, id, null));
                                                Extensions.CreateAndAdd(ref commaList, Token(SyntaxKind.CommaToken, token.Index));
                                                break;
                                            case ')':
                                                ++_tokenIndex;
                                                Extensions.CreateAndAdd(ref paraList, SyntaxFactory.Parameter(default(SyntaxList<AttributeListSyntax>),
                                                     default(SyntaxTokenList), type, id, null));
                                                closeParen = Token(SyntaxKind.CloseParenToken, token.Index);
                                                arrow = TokenExpected((int)TokenKind.EqualsGreaterThan);
                                                goto END;
                                            default:
                                                if (paraList == null)
                                                {
                                                    return null;
                                                }
                                                ErrorAndThrow(") expected.");
                                                break;
                                        }
                                    }
                                    break;
                                default:
                                    if (paraList == null)
                                    {
                                        return null;
                                    }
                                    ++_tokenIndex;
                                    IdentifierExpectedErrorAndThrow();
                                    break;
                            }
                        }
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
                    default:
                        if (paraList == null)
                        {
                            return null;
                        }
                        IdentifierExpectedErrorAndThrow();
                        break;
                }

            }
        END:
            return SyntaxFactory.ParenthesizedLambdaExpression(asyncTokenIndex == null ? default(SyntaxToken) : Token(SyntaxKind.AsyncKeyword, asyncTokenIndex.Value),
                SyntaxFactory.ParameterList(Token(SyntaxKind.OpenParenToken, openParenTokenIndex),
                    paraList == null ? default(SeparatedSyntaxList<ParameterSyntax>) : SyntaxFactory.SeparatedList(paraList, commaList), closeParen),
                arrow, LambdaExpressionBody());
        }
        #endregion lambda expr
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
                                        if (!TypeCore(out type, false, true))
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
            switch (token.Kind)
            {
                case (int)TokenKind.String:
                    ++_tokenIndex;
                    result = SyntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression,
                        SyntaxFactory.Literal(token.Value).Attach(token.Index));
                    break;
                case (int)TokenKind.Char:
                    ++_tokenIndex;
                    result = SyntaxFactory.LiteralExpression(SyntaxKind.CharacterLiteralExpression,
                        SyntaxFactory.Literal(token.Value[0]).Attach(token.Index));
                    break;
                case (int)TokenKind.Number:
                    ++_tokenIndex;
                    result = SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,
                        SyntaxFactory.ParseToken(token.Value).Attach(token.Index));
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
                            {
                                ++_tokenIndex;
                                var openParen = TokenExpected('(');
                                TypeSyntax type;
                                if (!TypeCore(out type, true))
                                {
                                    TypeExpectedErrorAndThrow();
                                }
                                result = SyntaxFactory.TypeOfExpression(Token(SyntaxKind.TypeOfKeyword, token.Index),
                                    openParen, type, TokenExpected(')'));
                                break;
                            }
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
                    if (NameStart(out n, false, true))
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
        private InitializerExpressionSyntax ArrayInitializer()
        {
            SyntaxToken st;
            if (Token('{', out st))
            {
                return SyntaxFactory.InitializerExpression(SyntaxKind.ArrayInitializerExpression, st,
                    SeparatedList(_ArrayInitializerExpressionCreator, null, true), TokenExpected('}'));
            }
            return null;
        }
        private bool ArrayInitializerExpression(out ExpressionSyntax result)
        {
            if (PeekToken(0, '{'))
            {
                result = ArrayInitializer();
                return true;
            }
            return Expression(out result);
        }
        #endregion expressions
        #region types
        protected bool Type(out TypeSyntax result)
        {
            return TypeCore(out result);
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
        protected bool TypeCore(out TypeSyntax result, bool unboundTypeArgs = false, bool checkNullableFollower = false, bool usePredefinedTypes = true)
        {
            PredefinedTypeSyntax pt;
            if (usePredefinedTypes && PredefinedType(out pt))
            {
                result = pt;
            }
            else
            {
                NameSyntax ns;
                if (Name(out ns, unboundTypeArgs))
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
                if (checkNullableFollower)
                {
                    //C# spec ???
                    switch (GetToken(1).Kind)
                    {
                        case ';':
                        case ',':
                        case ')':
                        case '[':
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
                result = SyntaxFactory.ArrayType(result, List(_ArrayRankSpecifierCreator));
            }
            return true;
        }
        protected bool PredefinedType(out PredefinedTypeSyntax result)
        {
            var token = GetToken();
            if (token.IsPredefinedType)
            {
                result = SyntaxFactory.PredefinedType(Token(token.SyntaxKind, token.Index));
                return true;
            }
            result = null;
            return false;
        }

        private bool ArrayRankSpecifier(out ArrayRankSpecifierSyntax result)
        {
            SyntaxToken st;
            if (Token('[', out st))
            {
                result = SyntaxFactory.ArrayRankSpecifier(st,
                    SeparatedList<ExpressionSyntax>(null, _OmittedArraySizeExpressionCreator), TokenExpected(']'));
                return true;
            }
            result = null;
            return false;
        }
        private bool ArrayRankSpecifierWithExpr(out ArrayRankSpecifierSyntax result)
        {
            SyntaxToken st;
            if (Token('[', out st))
            {
                result = SyntaxFactory.ArrayRankSpecifier(st,
                    SeparatedList(_ExpressionCreator, _OmittedArraySizeExpressionCreator), TokenExpected(']'));
                return true;
            }
            result = null;
            return false;
        }


        protected NameSyntax NameExpected()
        {
            NameSyntax result;
            if (!Name(out result))
            {
                ErrorAndThrow("Name expected.");
            }
            return result;
        }
        protected bool Name(out NameSyntax result, bool unboundTypeArgs = false)
        {
            if (!NameStart(out result))
            {
                return false;
            }
            SyntaxToken dot;
            while (Token('.', out dot))
            {
                result = SyntaxFactory.QualifiedName(result, dot, SimpleNameExpected(unboundTypeArgs));
            }
            return true;
        }
        protected bool NameStart(out NameSyntax result, bool unboundTypeArgs = false, bool checkTypeArgsFollower = false)
        {
            var idToken = GetToken();
            if (idToken.IsIdentifier)
            {
                var colonColonToken = GetToken(1);
                if (colonColonToken.TokenKind == TokenKind.ColonColon)
                {
                    IdentifierNameSyntax alias;
                    if (idToken.IsContextualKeyword("global"))
                    {
                        alias = SyntaxFactory.IdentifierName(Token(SyntaxKind.GlobalKeyword, idToken.Index));
                    }
                    else
                    {
                        alias = SyntaxFactory.IdentifierName(Identifier(idToken));
                    }
                    _tokenIndex += 2;
                    result = SyntaxFactory.AliasQualifiedName(alias, Token(SyntaxKind.ColonColonToken, colonColonToken.Index),
                        SimpleNameExpected(unboundTypeArgs, checkTypeArgsFollower));
                    return true;
                }
                SimpleNameSyntax sn;
                SimpleName(out sn, unboundTypeArgs, checkTypeArgsFollower);
                result = sn;
                return true;
            }
            result = null;
            return false;
        }

        protected bool SimpleName(out SimpleNameSyntax result, bool unboundTypeArgs = false, bool checkTypeArgsFollower = false)
        {
            var idToken = GetToken();
            if (idToken.IsIdentifier)
            {
                ++_tokenIndex;
                var ltToken = GetToken();
                if (ltToken.Kind == '<')
                {
                    if (checkTypeArgsFollower)
                    {
                        var tkIdx = _tokenIndex;
                        ++_tokenIndex;
                        var typeArgs = SeparatedList(_TypeCreator);
                        if (typeArgs.Count > 0)
                        {
                            SyntaxToken gtToken;
                            if (Token('>', out gtToken))
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
                                        result = SyntaxFactory.GenericName(Identifier(idToken),
                                            SyntaxFactory.TypeArgumentList(Token(SyntaxKind.LessThanToken, ltToken.Index), typeArgs, gtToken));
                                        return true;
                                }
                            }
                        }
                        _tokenIndex = tkIdx;
                    }
                    else
                    {
                        ++_tokenIndex;
                        var typeArgs = SeparatedList(_TypeCreator, unboundTypeArgs ? _OmittedTypeArgumentCreator : null);
                        if (typeArgs.Count == 0 && !unboundTypeArgs)
                        {
                            TypeExpectedErrorAndThrow();
                        }
                        result = SyntaxFactory.GenericName(Identifier(idToken),
                            SyntaxFactory.TypeArgumentList(Token(SyntaxKind.LessThanToken, ltToken.Index), typeArgs, TokenExpected('>')));
                        return true;
                    }

                }
                result = SyntaxFactory.IdentifierName(Identifier(idToken));
                return true;
            }
            result = null;
            return false;
        }
        protected SimpleNameSyntax SimpleNameExpected(bool unboundTypeArgs = false, bool checkTypeArgsFollower = false)
        {
            SimpleNameSyntax result;
            if (!SimpleName(out result, unboundTypeArgs, checkTypeArgsFollower))
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
        protected IdentifierNameSyntax IdentifierNameExpected()
        {
            IdentifierNameSyntax result;
            if (!IdentifierName(out result))
            {
                IdentifierExpectedErrorAndThrow();
            }
            return result;
        }
        #endregion types


    }

}

using System;
using Microsoft.CodeAnalysis.CSharp;

namespace CSharpParser
{
    public enum TokenKind
    {
        SingleLineComment = -300,//internal use
        ReservedKeyword,//class, int, true, new, etc
        NormalIdentifier,
        VerbatimIdentifier,// @id
        String,
        Char,
        Number,
        //
        BarBar,// ||
        BarEquals,// |=
        AmpersandAmpersand,// &&
        AmpersandEquals,// &=
        MinusMinus,// --
        MinusEquals,// -=
        MinusGreaterThan,// ->
        PlusPlus,// ++
        PlusEquals,// +=
        ExclamationEquals,// !=
        EqualsEquals,// ==
        EqualsGreaterThan,// =>
        LessThanEquals,// <=
        LessThanLessThan,// <<
        LessThanLessThanEquals,// <<=
        GreaterThanEquals,// >=
        //GreaterThanGreaterThan,// >> (>|> not recognized by lexer)
        //GreaterThanGreaterThanEquals,// >>= (>|>= not recognized by lexer)
        SlashEquals,// /=
        AsteriskEquals,// *=
        CaretEquals,// ^=
        PercentEquals,// %=
        QuestionQuestion,// ??
        ColonColon,// ::
        //
        //DollarDollar,// $$

    }
    public struct Token : IEquatable<Token>
    {
        public Token(int kind, string value, TextSpan textSpan, SyntaxKind syntaxKind = SyntaxKind.None)
        {
            Kind = kind;
            Value = value;
            TextSpan = textSpan;
            SyntaxKind = syntaxKind;
            Index = -1;
        }
        public readonly int Kind;
        public readonly string Value;//for TokenKind.ReservedKeyword to TokenKind.Number
        public readonly TextSpan TextSpan;
        public readonly SyntaxKind SyntaxKind;//for TokenKind.ReservedKeyword
        public int Index;
        public TokenKind TokenKind
        {
            get
            {
                return (TokenKind)Kind;
            }
        }
        public bool IsValid
        {
            get
            {
                return TextSpan.IsValid;
            }
        }
        public bool IsEndOfFile
        {
            get
            {
                return Kind == char.MaxValue;
            }
        }
        public bool IsReservedKeyword
        {
            get
            {
                return TokenKind == TokenKind.ReservedKeyword;
            }
        }
        public bool IsNormalIdentifier
        {
            get
            {
                return TokenKind == TokenKind.NormalIdentifier;
            }
        }
        public bool IsVerbatimIdentifier
        {
            get
            {
                return TokenKind == TokenKind.VerbatimIdentifier;
            }
        }
        public bool IsIdentifier
        {
            get
            {
                return IsNormalIdentifier || IsVerbatimIdentifier;
            }
        }
        public bool IsPredefinedType
        {
            get
            {
                return SyntaxFacts.IsPredefinedType(SyntaxKind);
            }
        }
        public bool IsContextualKeyword(string value)
        {
            return IsNormalIdentifier && Value == value;
        }
        //public bool IsContextualKeyword(string value1, string value2)
        //{
        //    return IsNormalIdentifier && (Value == value1 || Value == value2);
        //}

        //
        public override string ToString()
        {
            return Value;
        }
        public bool Equals(Token other)
        {
            return Value == other.Value;
        }
        public override bool Equals(object obj)
        {
            return obj is Token && Equals((Token)obj);
        }
        public override int GetHashCode()
        {
            return Value != null ? Value.GetHashCode() : 0;
        }
        public static bool operator ==(Token left, Token right)
        {
            return left.Equals(right);
        }
        public static bool operator !=(Token left, Token right)
        {
            return !left.Equals(right);
        }



    }

}

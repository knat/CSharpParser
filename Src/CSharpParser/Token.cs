
using System;

namespace CSharpParser
{
    public enum TokenKind
    {
        SingleLineComment = -1000,//internal use
        NormalIdentifier,
        VerbatimIdentifier,// @id
        NormalString,
        VerbatimString,// @"..."
        Char,// 'c'
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
        GreaterThanGreaterThan,// >> (>|> not recognized by lexer)
        GreaterThanGreaterThanEquals,// >>= (>|>= not recognized by lexer)
        SlashEquals,// /=
        AsteriskEquals,// *=
        CaretEquals,// ^=
        PercentEquals,// %=
        QuestionQuestion,// ??
        ColonColon,// ::
        //
        //HashHash,// ##

    }
    public struct Token : IEquatable<Token>
    {
        public Token(int kind, string value, TextSpan textSpan)
        {
            Kind = kind;
            Value = value;
            TextSpan = textSpan;
        }
        public readonly int Kind;
        public readonly string Value;//for TokenKind.NormalIdentifier to TokenKind.Number
        public readonly TextSpan TextSpan;
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
                return IsNormalIdentifier|| IsVerbatimIdentifier;
            }
        }
        public bool IsKeyword(string value)
        {
            return IsNormalIdentifier && Value == value;
        }

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

using System;
using Microsoft.CodeAnalysis.CSharp;

namespace CSharpParser
{
    public static class TokenKind
    {
        public const int SingleLineComment = -400;//internal use
        public const int ReservedKeyword = -399;//class, int, true, new, etc
        public const int NormalIdentifier = -398;
        public const int VerbatimIdentifier = -397;// @id
        public const int String = -396;
        public const int Char = -395;
        public const int Number = -394;
        //
        public const int BarBar = -300;// ||
        public const int BarEquals = -299;// |=
        public const int AmpersandAmpersand = -298;// &&
        public const int AmpersandEquals = -297;// &=
        public const int MinusMinus = -296;// --
        public const int MinusEquals = -295;// -=
        public const int MinusGreaterThan = -294;// ->
        public const int PlusPlus = -293;// ++
        public const int PlusEquals = -292;// +=
        public const int ExclamationEquals = -291;// !=
        public const int EqualsEquals = -290;// ==
        public const int EqualsGreaterThan = -289;// =>
        public const int LessThanEquals = -288;// <=
        public const int LessThanLessThan = -287;// <<
        public const int LessThanLessThanEquals = -286;// <<=
        public const int GreaterThanEquals = -285;// >=
        //GreaterThanGreaterThan,// >> (>|> not recognized by lexer)
        //GreaterThanGreaterThanEquals,// >>= (>|>= not recognized by lexer)
        public const int SlashEquals = -282;// /=
        public const int AsteriskEquals = -281;// *=
        public const int CaretEquals = -280;// ^=
        public const int PercentEquals = -279;// %=
        public const int QuestionQuestion = -278;// ??
        public const int ColonColon = -277;// ::
        //
        public const int DollarDollar = -200;// $$

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
                return Kind == TokenKind.ReservedKeyword;
            }
        }
        public bool IsNormalIdentifier
        {
            get
            {
                return Kind == TokenKind.NormalIdentifier;
            }
        }
        public bool IsVerbatimIdentifier
        {
            get
            {
                return Kind == TokenKind.VerbatimIdentifier;
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

//#define ddd /*fdsf*/
# region
#if false

#else

#endif
#endregion

namespace CSharpParser {
    public enum TokenKind {
        //PpHash = -1000,// preprocessor directive '#', internal use
        //Whitespace,
        //NewLine,
        //MultiLineComment,
        SingleLineComment = -1000,//internal use
        NormalIdentifier,
        VerbatimIdentifier,// @id
        NormalString,
        VerbatimString,// @"..."
        Char,// 'c'
        IntegerLiteral,// 123
        DecimalLiteral,// 123.45
        RealLiteral,// 123.45Ee+-12
        //
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
        //GreaterThanGreaterThan,// >>
        //GreaterThanGreaterThanEquals,// >>=
        SlashEquals,// /=
        AsteriskEquals,// *=
        CaretEquals,// ^=
        PercentEquals,// %=
        QuestionQuestion,// ??
        ColonColon,// ::
        //
        HashHash,// ##

    }
    public struct Token {
        public Token(int kind, string value, TextSpan textSpan) {
            Kind = kind;
            Value = value;
            TextSpan = textSpan;
        }
        public readonly int Kind;
        public readonly string Value;//for TokenKind.Identifier to TokenKind.RealValue
        public readonly TextSpan TextSpan;
        public TokenKind TokenKind {
            get {
                return (TokenKind)Kind;
            }
        }
        //public bool IsWhitespace {
        //    get {
        //        return TokenKind == TokenKind.Whitespace;
        //    }
        //}
        //public bool IsNewLine {
        //    get {
        //        return TokenKind == TokenKind.NewLine;
        //    }
        //}
        public bool IsSingleLineComment {
            get {
                return TokenKind == TokenKind.SingleLineComment;
            }
        }
        public bool IsNormalIdentifier {
            get {
                return TokenKind == TokenKind.NormalIdentifier;
            }
        }
        public bool IsVerbatimIdentifier {
            get {
                return TokenKind == TokenKind.VerbatimIdentifier;
            }
        }

        public bool IsEndOfFile {
            get {
                return Kind == char.MaxValue;
            }
        }
    }

}

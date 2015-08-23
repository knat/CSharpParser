using Microsoft.CodeAnalysis;
using System.Collections.Generic;
using System.Globalization;

namespace CSharpParser
{
    public static class Extensions
    {
        public const string SystemUri = "https://github.com/knat/CSharpParser";
        //
        //
        //
        //public static string InvFormat(this string format, params string[] args)
        //{
        //    return StringBuilderBuffer.Acquire().AppendFormat(CultureInfo.InvariantCulture, format, args).ToStringAndRelease();
        //}
        public const int ParsingErrorCode = 1;

        internal static string ToInvString(this int value)
        {
            return value.ToString(CultureInfo.InvariantCulture);
        }
        private const string _syntaxAnnotationKindName = "CSPTokenIndex";
        public static SyntaxToken Attach(this SyntaxToken syntaxToken, int tokenIndex)
        {
            return syntaxToken.WithAdditionalAnnotations(new SyntaxAnnotation(_syntaxAnnotationKindName, tokenIndex.ToInvString()));
        }
        public static SyntaxToken Attach(this SyntaxToken syntaxToken, int tokenIndex1, int tokenIndex2)
        {
            return syntaxToken.WithAdditionalAnnotations(new SyntaxAnnotation(_syntaxAnnotationKindName,
                "_" + tokenIndex1.ToInvString() + "_" + tokenIndex2.ToInvString()));
        }



        //
        //public static int AggregateHash(int hash, int newValue)
        //{
        //    unchecked
        //    {
        //        return hash * 31 + newValue;
        //    }
        //}
        //public static int CombineHash(int a, int b)
        //{
        //    unchecked
        //    {
        //        int hash = 17;
        //        hash = hash * 31 + a;
        //        hash = hash * 31 + b;
        //        return hash;
        //    }
        //}
        //public static int CombineHash(int a, int b, int c)
        //{
        //    unchecked
        //    {
        //        int hash = 17;
        //        hash = hash * 31 + a;
        //        hash = hash * 31 + b;
        //        hash = hash * 31 + c;
        //        return hash;
        //    }
        //}
        //
        internal static void CreateAndAdd<T>(ref List<T> list, T item)
        {
            if (list == null)
            {
                list = new List<T>();
            }
            list.Add(item);
        }
        //internal static int CountOrZero<T>(this List<T> list) {
        //    return list == null ? 0 : list.Count;
        //}

        //
        //

    }
}

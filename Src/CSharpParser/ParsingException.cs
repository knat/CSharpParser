using System;

namespace CSharpParser
{
    public sealed class ParsingException : Exception
    {
        private ParsingException()
        {
        }
        public static readonly ParsingException Instance = new ParsingException();
    }
}

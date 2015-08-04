using System;

namespace CSharpParser {
    public sealed class ParsingException : Exception {
        private ParsingException() {
        }
        internal static readonly ParsingException Instance = new ParsingException();
    }
}

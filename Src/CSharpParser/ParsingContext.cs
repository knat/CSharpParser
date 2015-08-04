using System.Collections.Generic;

namespace CSharpParser {
    public class ParsingContext {
        public ParsingContext() {
            DiagnosticList = new List<Diagnostic>();
            _tokenList = new List<Token>();
        }
        public readonly List<Diagnostic> DiagnosticList;
        private readonly List<Token> _tokenList;
        public virtual void Reset() {
            DiagnosticList.Clear();
            _tokenList.Clear();
        }

        public void AddDiag(DiagnosticSeverity severity, int code, string message, TextSpan textSpan) {
            DiagnosticList.Add(new Diagnostic(severity, code, message, textSpan));
        }
        //public void AddDiag(DiagnosticSeverity severity, DiagMsg diagMsg, TextSpan textSpan) {
        //    DiagList.Add(new Diagnostic(severity, diagMsg, textSpan));
        //}
        public bool HasDiags {
            get {
                return DiagnosticList.Count > 0;
            }
        }
        public bool HasErrorDiags {
            get {
                return HasErrorDiagsCore(0);
            }
        }
        private bool HasErrorDiagsCore(int startIndex) {
            var list = DiagnosticList;
            var count = list.Count;
            for (; startIndex < count; ++startIndex) {
                if (list[startIndex].IsError) {
                    return true;
                }
            }
            return false;
        }
        public struct Marker {
            internal Marker(ParsingContext context) {
                Context = context;
                StartIndex = context.DiagnosticList.Count;
            }
            internal readonly ParsingContext Context;
            public readonly int StartIndex;
            public int Count {
                get {
                    return Context.DiagnosticList.Count - StartIndex;
                }
            }
            public bool HasErrorDiags {
                get {
                    return Context.HasErrorDiagsCore(StartIndex);
                }
            }
            public void Restore() {
                Context.DiagnosticList.RemoveRange(StartIndex, Context.DiagnosticList.Count - StartIndex);
            }
        }
        public Marker Mark() {
            return new Marker(this);
        }
        //
        internal string AddToken(Token token) {
            var list = _tokenList;
            var idxStr = list.Count.ToInvString();
            list.Add(token);
            return idxStr;
        }

    }


}

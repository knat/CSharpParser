using System.Collections.Generic;

namespace CSharpParser
{
    public class ParsingContext
    {
        public ParsingContext()
        {
            DiagnosticList = new List<Diagnostic>();
            TokenList = new List<Token>();
        }
        public readonly List<Diagnostic> DiagnosticList;
        public readonly List<Token> TokenList;
        public virtual void Reset()
        {
            DiagnosticList.Clear();
            TokenList.Clear();
        }

        public void AddDiag(DiagnosticSeverity severity, int code, string message, TextSpan textSpan)
        {
            DiagnosticList.Add(new Diagnostic(severity, code, message, textSpan));
        }
        public bool HasDiagnostics
        {
            get
            {
                return DiagnosticList.Count > 0;
            }
        }
        public bool HasErrorDiagnostics
        {
            get
            {
                return HasErrorDiagnosticsCore(0);
            }
        }
        private bool HasErrorDiagnosticsCore(int startIndex)
        {
            var list = DiagnosticList;
            var count = list.Count;
            for (; startIndex < count; ++startIndex)
            {
                if (list[startIndex].IsError)
                {
                    return true;
                }
            }
            return false;
        }
        //public struct Marker
        //{
        //    internal Marker(ParsingContext context)
        //    {
        //        Context = context;
        //        StartIndex = context.DiagnosticList.Count;
        //    }
        //    public readonly ParsingContext Context;
        //    public readonly int StartIndex;
        //    public int Count
        //    {
        //        get
        //        {
        //            return Context.DiagnosticList.Count - StartIndex;
        //        }
        //    }
        //    public bool HasErrorDiags
        //    {
        //        get
        //        {
        //            return Context.HasErrorDiagnosticsCore(StartIndex);
        //        }
        //    }
        //    public void Restore()
        //    {
        //        Context.DiagnosticList.RemoveRange(StartIndex, Context.DiagnosticList.Count - StartIndex);
        //    }
        //}
        //public Marker Mark()
        //{
        //    return new Marker(this);
        //}
        //
        //internal string AddToken(Token token)
        //{
        //    var list = _tokenList;
        //    var idxStr = list.Count.ToInvString();
        //    list.Add(token);
        //    return idxStr;
        //}

    }


}

using System;
using System.Runtime.Serialization;

namespace CSharpParser
{
    public enum DiagnosticCode
    {
        None = 0,
        Parsing = -1000,

        //del
        //UriReserved,
        //AliasReserved,
        //DuplicateAlias,
        //InvalidUriReference,
        //InvalidAliasReference,
        //AmbiguousGlobalTypeReference,
        //InvalidGlobalTypeReference,

        //DuplicateExpressionArgumentName,
        //DuplicateLambdaParameterName,

        //InvalidClassReference,
        //ClassNotEqualToOrDeriveFromTheDeclared,
        //ClassIsAbstract,
        //InvalidPropertyName,
        //PropertyMissing,
        //NullNotAllowed,
        //ValueExpected,
        //SpecificValueExpected,
        //InvalidAtomValue,
        //InvalidEnumReference,
        //EnumNotEqualToTheDeclared,
        //InvalidEnumMemberName,
        //DuplicateSetItem,
        //DuplicateMapKey,

    }

    //public struct DiagMsg
    //{
    //    public DiagMsg(DiagnosticCode code)
    //    {
    //        Code = code;
    //        _msgArgs = null;
    //    }
    //    public DiagMsg(DiagnosticCode code, params string[] msgArgs)
    //    {
    //        Code = code;
    //        _msgArgs = msgArgs;
    //    }
    //    public readonly DiagnosticCode Code;
    //    private readonly string[] _msgArgs;
    //    public string GetMessage()
    //    {
    //        switch (Code)
    //        {
    //            case DiagnosticCode.UriReserved:
    //                return "Uri '" + Extensions.SystemUri + "' is reserved.";
    //            case DiagnosticCode.AliasReserved:
    //                return "Alias 'sys' or 'thisns' are reserved.";
    //            case DiagnosticCode.DuplicateAlias:
    //                return "Duplicate alias '{0}'.".InvFormat(_msgArgs);
    //            case DiagnosticCode.InvalidUriReference:
    //                return "Invalid uri reference '{0}'.".InvFormat(_msgArgs);
    //            case DiagnosticCode.InvalidAliasReference:
    //                return "Invalid alias reference '{0}'.".InvFormat(_msgArgs);
    //            case DiagnosticCode.AmbiguousGlobalTypeReference:
    //                return "Ambiguous global type reference '{0}'.".InvFormat(_msgArgs);
    //            case DiagnosticCode.InvalidGlobalTypeReference:
    //                return "Invalid global type reference '{0}'.".InvFormat(_msgArgs);
    //            case DiagnosticCode.DuplicateExpressionArgumentName:
    //                return "Duplicate expression argument name '{0}'.".InvFormat(_msgArgs);
    //            case DiagnosticCode.DuplicateLambdaParameterName:
    //                return "Duplicate lambda parameter name '{0}'.".InvFormat(_msgArgs);


    //            case DiagnosticCode.InvalidClassReference:
    //                return "Invalid class reference '{0}'.".InvFormat(_msgArgs);
    //            case DiagnosticCode.ClassNotEqualToOrDeriveFromTheDeclared:
    //                return "Class '{0}' not equal to or derive from the declared class '{1}'.".InvFormat(_msgArgs);
    //            case DiagnosticCode.ClassIsAbstract:
    //                return "Class '{0}' is abstract.".InvFormat(_msgArgs);
    //            case DiagnosticCode.InvalidPropertyName:
    //                return "Invalid property name '{0}'.".InvFormat(_msgArgs);
    //            case DiagnosticCode.PropertyMissing:
    //                return "Property '{0}' missing.".InvFormat(_msgArgs);
    //            case DiagnosticCode.NullNotAllowed:
    //                return "Null not allowed.";
    //            case DiagnosticCode.ValueExpected:
    //                return "Value expetced.";
    //            case DiagnosticCode.SpecificValueExpected:
    //                return "{0} value expetced.".InvFormat(_msgArgs);
    //            case DiagnosticCode.InvalidAtomValue:
    //                return "Invalid atom '{0}' value '{1}'.".InvFormat(_msgArgs);
    //            case DiagnosticCode.InvalidEnumReference:
    //                return "Invalid enum reference '{0}'.".InvFormat(_msgArgs);
    //            case DiagnosticCode.EnumNotEqualToTheDeclared:
    //                return "Enum '{0}' not equal to the declared enum '{1}'.".InvFormat(_msgArgs);
    //            case DiagnosticCode.InvalidEnumMemberName:
    //                return "Invalid enum member name '{0}'.".InvFormat(_msgArgs);
    //            case DiagnosticCode.DuplicateSetItem:
    //                return "Duplicate set item.";
    //            case DiagnosticCode.DuplicateMapKey:
    //                return "Duplicate map key.";

    //            default:
    //                throw new InvalidOperationException("Invalid code: " + Code.ToString());
    //        }
    //    }
    //}

    public enum DiagnosticSeverity : byte
    {
        None = 0,
        Error = 1,
        Warning = 2,
        Info = 3
    }

    [DataContract(Namespace = Extensions.SystemUri)]
    public struct Diagnostic
    {
        public Diagnostic(DiagnosticSeverity severity, int code, string message, TextSpan textSpan)
        {
            Severity = severity;
            Code = code;
            Message = message;
            TextSpan = textSpan;
        }
        //public Diagnostic(DiagnosticSeverity severity, DiagMsg diagMsg, TextSpan textSpan)
        //    : this(severity, (int)diagMsg.Code, diagMsg.GetMessage(), textSpan) {
        //}
        [DataMember]
        public readonly DiagnosticSeverity Severity;
        [DataMember]
        public readonly int Code;
        [DataMember]
        public readonly string Message;
        [DataMember]
        public readonly TextSpan TextSpan;//opt
        public bool IsError
        {
            get
            {
                return Severity == DiagnosticSeverity.Error;
            }
        }
        public bool IsWarning
        {
            get
            {
                return Severity == DiagnosticSeverity.Warning;
            }
        }
        public bool IsInfo
        {
            get
            {
                return Severity == DiagnosticSeverity.Info;
            }
        }
        //internal DiagnosticCode DiagCode
        //{
        //    get
        //    {
        //        return (DiagnosticCode)Code;
        //    }
        //}
        public bool HasTextSpan
        {
            get
            {
                return TextSpan.IsValid;
            }
        }
        public bool IsValid
        {
            get
            {
                return Severity != DiagnosticSeverity.None;
            }
        }
        public override string ToString()
        {
            if (IsValid)
            {
                var sb = StringBuilderBuffer.Acquire();
                sb.Append(Severity.ToString());
                sb.Append(' ');
                sb.Append(Code.ToInvString());
                sb.Append(": ");
                sb.Append(Message);
                if (HasTextSpan)
                {
                    sb.Append("\r\n    ");
                    sb.Append(TextSpan.ToString());
                }
                return sb.ToStringAndRelease();
            }
            return null;
        }
    }

    //[CollectionDataContract(Namespace = Extensions.SystemUri)]



}

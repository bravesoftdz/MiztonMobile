{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2010 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

unit SysMobile; { Predefined constants, types, procedures, }
             { and functions (such as True, Integer, or }
             { Writeln) do not have actual declarations.}
             { Instead they are built into the compiler }
             { and are treated as if they were declared }
             { at the beginning of the System unit.     }

{$H+,I-,R-,O+,W-}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN SYMBOL_DEPRECATED OFF}
{$WARN UNSAFE_TYPE OFF}



{ L- should never be specified.

  The IDE needs to find DebugHook (through the C++
  compiler sometimes) for integrated debugging to
  function properly.

  ILINK will generate debug info for DebugHook if
  the object module has not been compiled with debug info.

  ILINK will not generate debug info for DebugHook if
  the object module has been compiled with debug info.

  Thus, the Pascal compiler must be responsible for
  generating the debug information for that symbol
  when a debug-enabled object file is produced.
}

interface

{$IFNDEF CPU386}
  {$DEFINE PUREPASCAL}
{$ENDIF}

(* You can use RTLVersion in $IF expressions to test the runtime library
  version level independently of the compiler version level.
  Example:  {$IF RTLVersion >= 16.2} ... {$IFEND}                  *)

const
  RTLVersion = 22.00;

{$EXTERNALSYM CompilerVersion}

(*
const
  CompilerVersion = 0.0;

  CompilerVersion is assigned a value by the compiler when
  the system unit is compiled.  It indicates the revision level of the
  compiler features / language syntax, which may advance independently of
  the RTLVersion.  CompilerVersion can be tested in $IF expressions and
  should be used instead of testing for the VERxxx conditional define.
  Always test for greater than or less than a known revision level.
  It's a bad idea to test for a specific revision level.
*)

{$IFDEF DECLARE_GPL}
(* The existence of the GPL symbol indicates that the System unit
  and the rest of the Delphi runtime library were compiled for use
  and distribution under the terms of the GNU General Public License (GPL).
  Under the terms of the GPL, all applications compiled with the
  GPL version of the Delphi runtime library must also be distributed
  under the terms of the GPL.
  For more information about the GNU GPL, see
  http://www.gnu.org/copyleft/gpl.html

  The GPL symbol does not exist in the Delphi runtime library
  purchased for commercial/proprietary software development.

  If your source code needs to know which licensing model it is being
  compiled into, you can use {$IF DECLARED(GPL)}...{$IFEND} to
  test for the existence of the GPL symbol.  The value of the
  symbol itself is not significant.   *)

const
  GPL = True;
{$ENDIF}

{ Delphi built-in types for .hpp/.obj support }
{   Most of built-in types are defined in sysmac.h }
{   Pointer types should be mangled by the compiler for constness}
{$EXTERNALSYM Boolean     'bool'             } {$OBJTYPENAME Boolean    'Bo'}
{$NODEFINE    ShortInt    'ShortInt'         } {$OBJTYPENAME ShortInt   'Bzc'} { signed char }
{-EXTERNALSYM ShortInt    'signed char'      } {-OBJTYPENAME ShortInt   'Bzc'}
{$EXTERNALSYM SmallInt    'short'            } {$OBJTYPENAME SmallInt   'Bs'}
{$EXTERNALSYM Integer     'int'              } {$OBJTYPENAME Integer    'Bi'}
{$NODEFINE    Byte        'Byte'             } {$OBJTYPENAME Byte       'Buc'} { unsigned char }
{$NODEFINE    Word        'Word'             } {$OBJTYPENAME Word       'Bus'} { unsigned short }
{$EXTERNALSYM Cardinal    'unsigned'         } {$OBJTYPENAME Cardinal   'Bui'}
{$EXTERNALSYM Int64       '__int64'          } {$OBJTYPENAME Int64      'Bj'}
{$EXTERNALSYM UInt64      'unsigned __int64' } {$OBJTYPENAME UInt64     'Buj'}
{$EXTERNALSYM NativeInt   'int'              } {$OBJTYPENAME NativeInt  'Bi'}
{$EXTERNALSYM NativeUInt  'unsigned'         } {$OBJTYPENAME NativeUInt 'Bui'}
{$EXTERNALSYM Single      'float'            } {$OBJTYPENAME Single     'Bf'}
{$EXTERNALSYM Double      'double'           } {$OBJTYPENAME Double     'Bd'}
{$NODEFINE    Extended    'Extended'         } {$OBJTYPENAME Extended   'Bg'} { long double }
{$NODEFINE    Currency    'Currency'    'CurrencyBase'    } {$OBJTYPENAME Currency    'NCurrency'}
{$NODEFINE    Comp        'Comp'        'CompBase'        } {$OBJTYPENAME Comp        'NComp'}
{$EXTERNALSYM Real        'double'                        } {$OBJTYPENAME Real        'Bd'}
{$NODEFINE    ShortString 'ShortString' 'ShortStringBase' } {$OBJTYPENAME ShortString 'N%SmallString$iuc$255%'}
{$NODEFINE    OpenString  'OpenString'       } {$OBJTYPENAME OpenString 'Bxpc'} { char * const }
{$NODEFINE    File        'file'             } {$OBJTYPENAME File       'Nfile'}
{$NODEFINE    Text        'TextFile'         } {$OBJTYPENAME Text       'NTextfile'}
{$NODEFINE    ByteBool    'ByteBool'         } {$OBJTYPENAME ByteBool   'Buc'} { unsigned char }
{$NODEFINE    WordBool    'WordBool'         } {$OBJTYPENAME WordBool   'Bus'} { unsigned short }
{$EXTERNALSYM LongBool    'BOOL'             } {$OBJTYPENAME LongBool   'Bi'}  { int } { from windef.h }
{$NODEFINE    Real48      } { not supported in C++ }
{$EXTERNALSYM Pointer     'void *'    }
{$NODEFINE    PWideChar   'WideChar *'}
{$EXTERNALSYM PAnsiChar   'char *'    }
{$NODEFINE    Variant     } { defined in sysvari.h }
{$NODEFINE    OleVariant  } { defined in sysvari.h }
{$NODEFINE    LongInt     } { alias of Integer     }
{$NODEFINE    LongWord    } { alias of Cardinal    }
{$NODEFINE    TextFile    } { alias of Text        }
{$EXTERNALSYM AnsiChar     'char'          } {$OBJTYPENAME AnsiChar 'Bc'}
{$IFDEF MSWINDOWS}
  {$NODEFINE  Char         'WideChar'      } {$OBJTYPENAME Char     'Bb'}  { wchar_t }
{$ELSE}
  {$NODEFINE  Char         'WideChar'} {$OBJTYPENAME Char     'BCs'} { char16_t }
{$ENDIF}
{$NODEFINE    string       'UnicodeString' } {$OBJTYPENAME string   'NUnicodeString'} { defined in vcl/ustring.h }
{-NODEFINE    string       'String'        } {$OBJTYPENAME string   'NUnicodeString'} { defined in vcl/ustring.h }
{$NODEFINE    AnsiString   } { defined in vcl/dstring.h }
{$NODEFINE    WideString   } { defined in vcl/wstring.h }
{$NODEFINE    PChar        } { alias of PWideChar  }
{$NODEFINE    WideChar     } { alias of Char       }
{$NODEFINE    UnicodeString} { alias of string     }

(*$HPPEMIT 'namespace System' *)
(*$HPPEMIT '{' *)
(*$HPPEMIT '  typedef Shortint ShortInt;' *)
(*$HPPEMIT '  typedef Smallint SmallInt;' *)
(*$HPPEMIT '  typedef Longint LongInt;' *)
(*$HPPEMIT '}' *)

type
  CppLongInt  = type LongInt;  {$EXTERNALSYM CppLongInt  'long'         } {$OBJTYPENAME CppLongInt  'Bl'}
  CppULongInt = type LongWord; {$EXTERNALSYM CppULongInt 'unsigned long'} {$OBJTYPENAME CppULongInt 'Bul'}

{ Useful alias types }
type
  Int8   = ShortInt;
  Int16  = SmallInt;
  Int32  = Integer;
  UInt8  = Byte;
  UInt16 = Word;
  UInt32 = Cardinal;

const
{ Variant type codes (wtypes.h) }

  varEmpty    = $0000; { vt_empty        0 }
  varNull     = $0001; { vt_null         1 }
  varSmallint = $0002; { vt_i2           2 }
  varInteger  = $0003; { vt_i4           3 }
  varSingle   = $0004; { vt_r4           4 }
  varDouble   = $0005; { vt_r8           5 }
  varCurrency = $0006; { vt_cy           6 }
  varDate     = $0007; { vt_date         7 }
  varOleStr   = $0008; { vt_bstr         8 }
  varDispatch = $0009; { vt_dispatch     9 }
  varError    = $000A; { vt_error       10 }
  varBoolean  = $000B; { vt_bool        11 }
  varVariant  = $000C; { vt_variant     12 }
  varUnknown  = $000D; { vt_unknown     13 }
//varDecimal  = $000E; { vt_decimal     14 } {UNSUPPORTED as of v6.x code base}
//varUndef0F  = $000F; { undefined      15 } {UNSUPPORTED per Microsoft}
  varShortInt = $0010; { vt_i1          16 }
  varByte     = $0011; { vt_ui1         17 }
  varWord     = $0012; { vt_ui2         18 }
  varLongWord = $0013; { vt_ui4         19 }
  varInt64    = $0014; { vt_i8          20 }
  varUInt64   = $0015; { vt_ui8         21 }
{  if adding new items, update Variants' varLast, BaseTypeMap and OpTypeMap }

  varStrArg   = $0048; { vt_clsid        72 }
  varString   = $0100; { Pascal string  256 } {not OLE compatible }
  varAny      = $0101; { Corba any      257 } {not OLE compatible }
  varUString  = $0102; { Unicode string 258 } {not OLE compatible }
  // custom types range from $110 (272) to $7FF (2047)

  varTypeMask = $0FFF;
  varArray    = $2000;
  varByRef    = $4000;

{ TVarRec.VType values }

  vtInteger       = 0;
  vtBoolean       = 1;
  vtChar          = 2;
  vtExtended      = 3;
  vtString        = 4;
  vtPointer       = 5;
  vtPChar         = 6;
  vtObject        = 7;
  vtClass         = 8;
  vtWideChar      = 9;
  vtPWideChar     = 10;
  vtAnsiString    = 11;
  vtCurrency      = 12;
  vtVariant       = 13;
  vtInterface     = 14;
  vtWideString    = 15;
  vtInt64         = 16;
  vtUnicodeString = 17;

{ Virtual method table entries }
{$IF defined(CPUX64)}
  vmtSelfPtr           = -172;
  vmtIntfTable         = -168;
  vmtAutoTable         = -160;
  vmtInitTable         = -152;
  vmtTypeInfo          = -144;
  vmtFieldTable        = -136;
  vmtMethodTable       = -128;
  vmtDynamicTable      = -120;
  vmtClassName         = -112;
  vmtInstanceSize      = -104;
  vmtParent            = -96;
  vmtEquals            = -88 deprecated 'Use VMTOFFSET in asm code';
  vmtGetHashCode       = -80 deprecated 'Use VMTOFFSET in asm code';
  vmtToString          = -72 deprecated 'Use VMTOFFSET in asm code';
  vmtSafeCallException = -64 deprecated 'Use VMTOFFSET in asm code';
  vmtAfterConstruction = -56 deprecated 'Use VMTOFFSET in asm code';
  vmtBeforeDestruction = -48 deprecated 'Use VMTOFFSET in asm code';
  vmtDispatch          = -40 deprecated 'Use VMTOFFSET in asm code';
  vmtDefaultHandler    = -32 deprecated 'Use VMTOFFSET in asm code';
  vmtNewInstance       = -24 deprecated 'Use VMTOFFSET in asm code';
  vmtFreeInstance      = -16 deprecated 'Use VMTOFFSET in asm code';
  vmtDestroy           =  -8 deprecated 'Use VMTOFFSET in asm code';

  vmtQueryInterface    =  0 deprecated 'Use VMTOFFSET in asm code';
  vmtAddRef            =  8 deprecated 'Use VMTOFFSET in asm code';
  vmtRelease           = 16 deprecated 'Use VMTOFFSET in asm code';
  vmtCreateObject      = 24 deprecated 'Use VMTOFFSET in asm code';
{$ELSE !CPUX64}
  vmtSelfPtr           = -88;
  vmtIntfTable         = -84;
  vmtAutoTable         = -80;
  vmtInitTable         = -76;
  vmtTypeInfo          = -72;
  vmtFieldTable        = -68;
  vmtMethodTable       = -64;
  vmtDynamicTable      = -60;
  vmtClassName         = -56;
  vmtInstanceSize      = -52;
  vmtParent            = -48;
  vmtEquals            = -44 deprecated 'Use VMTOFFSET in asm code';
  vmtGetHashCode       = -40 deprecated 'Use VMTOFFSET in asm code';
  vmtToString          = -36 deprecated 'Use VMTOFFSET in asm code';
  vmtSafeCallException = -32 deprecated 'Use VMTOFFSET in asm code';
  vmtAfterConstruction = -28 deprecated 'Use VMTOFFSET in asm code';
  vmtBeforeDestruction = -24 deprecated 'Use VMTOFFSET in asm code';
  vmtDispatch          = -20 deprecated 'Use VMTOFFSET in asm code';
  vmtDefaultHandler    = -16 deprecated 'Use VMTOFFSET in asm code';
  vmtNewInstance       = -12 deprecated 'Use VMTOFFSET in asm code';
  vmtFreeInstance      = -8 deprecated 'Use VMTOFFSET in asm code';
  vmtDestroy           = -4 deprecated 'Use VMTOFFSET in asm code';

  vmtQueryInterface    = 0 deprecated 'Use VMTOFFSET in asm code';
  vmtAddRef            = 4 deprecated 'Use VMTOFFSET in asm code';
  vmtRelease           = 8 deprecated 'Use VMTOFFSET in asm code';
  vmtCreateObject      = 12 deprecated 'Use VMTOFFSET in asm code';
{$IFEND !CPUX64}

  { Hidden TObject field info }
  hfFieldSize          = SizeOf(Pointer);
  hfMonitorOffset      = 0;

{ RTTI Visibility }
type
  TVisibilityClasses = set of (vcPrivate, vcProtected, vcPublic, vcPublished);

const
  { These constants represent the default settings built into the compiler.
    For classes, these settings are normally inherited from TObject. }
  DefaultMethodRttiVisibility = [vcPublic, vcPublished];
  DefaultFieldRttiVisibility = [vcPrivate..vcPublished];
  DefaultPropertyRttiVisibility = [vcPublic, vcPublished];

type
  { Default RTTI settings }
  {$RTTI INHERIT
      METHODS(DefaultMethodRttiVisibility)
      FIELDS(DefaultFieldRttiVisibility)
      PROPERTIES(DefaultPropertyRttiVisibility)}

  { Minimal RTTI generation henceforth in this file }
  {.$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}

  TArray<T> = array of T;

  TObject = class;
  {$NODEFINE TObject}   { defined in systobj.h }

  TClass = class of TObject;
  {$NODEFINE TClass}    { defined in systobj.h }

  HRESULT = type Longint;  { from wtypes.h }
  {$EXTERNALSYM HRESULT} {$OBJTYPENAME HRESULT 'Bl'} { long }

  PGUID = ^TGUID;
  TGUID = packed record
    D1: LongWord;
    D2: Word;
    D3: Word;
    D4: array[0..7] of Byte;
    class operator Equal(const Left, Right: TGUID): Boolean;
    class operator NotEqual(const Left, Right: TGUID): Boolean;
    class function Empty: TGUID; static;
  end;
  {$NODEFINE PGUID}             { defined in sysmac.h }
  {$EXTERNALSYM TGUID 'GUID' }  { defined in sysmac.h }
  {$OBJTYPENAME TGUID 'N_GUID'}
  { Type 'GUID' in C++ is alias of '_GUID' and defined in guiddef.h (wtypes.h) }

  PInterfaceEntry = ^TInterfaceEntry;
  TInterfaceEntry = packed record
    IID: TGUID;
    VTable: Pointer;
    IOffset: Integer;
    {$IF defined(CPUX64)}
    _Filler: LongWord;
    {$IFEND}
    ImplGetter: NativeUInt;
  end;

  PInterfaceTable = ^TInterfaceTable;
  TInterfaceTable = packed record
    EntryCount: Integer;
    {$IF defined(CPUX64)}
    _Filler: LongWord;
    {$IFEND}
    Entries: array[0..9999] of TInterfaceEntry;
  end;

  TMethod = record
    Code, Data: Pointer;
  end;

{ TObject.Dispatch accepts any data type as its Message parameter.  The
  first 2 bytes of the data are taken as the message id to search for
  in the object's message methods.  TDispatchMessage is an example of
  such a structure with a word field for the message id.
}
  TDispatchMessage = record
    MsgID: Word;
  end;

  TObject = class
  public
    constructor Create;
    procedure Free;
    class function InitInstance(Instance: Pointer): TObject;
    procedure CleanupInstance;
    function ClassType: TClass; inline;
    class function ClassName: string;
    class function ClassNameIs(const Name: string): Boolean;
    class function ClassParent: TClass;
    class function ClassInfo: Pointer; inline;
    class function InstanceSize: Longint; inline;
    class function InheritsFrom(AClass: TClass): Boolean;
    class function MethodAddress(const Name: ShortString): Pointer; overload;
    class function MethodAddress(const Name: string): Pointer; overload;
    class function MethodName(Address: Pointer): string;
    function FieldAddress(const Name: ShortString): Pointer; overload;
    function FieldAddress(const Name: string): Pointer; overload;
    function GetInterface(const IID: TGUID; out Obj): Boolean;
    class function GetInterfaceEntry(const IID: TGUID): PInterfaceEntry;
    class function GetInterfaceTable: PInterfaceTable;
    class function UnitName: string;
    function Equals(Obj: TObject): Boolean; virtual;
    function GetHashCode: Integer; virtual;
    function ToString: string; virtual;
    function SafeCallException(ExceptObject: TObject;
      ExceptAddr: Pointer): HResult; virtual;
    procedure AfterConstruction; virtual;
    procedure BeforeDestruction; virtual;
    procedure Dispatch(var Message); virtual;
    procedure DefaultHandler(var Message); virtual;
    class function NewInstance: TObject; virtual;
    procedure FreeInstance; virtual;
    destructor Destroy; virtual;
  end;

{$IF Defined(MSWINDOWS)}
  TThreadID = LongWord;
{$IFEND}
{$IF Defined(MACOS) or Defined(LINUX)}
  TThreadID = NativeUInt;
{$IFEND}

  { TMonitor is an implementation of the concept invented by C.A.R Hoare and Per Brinch Hansen.
    See http://en.wikipedia.org/wiki/Monitor_%28synchronization%29 for more information.

    Every TObject derived instance can be used as a monitor. However, it is recommended that privately
    constructed objects be used rather than the publicly available instance itself. This will allow the
    developer to better control access to the lock and to ensure that the locking rules are adhered to.
    If a publicly available instance were to be used a the lock, such as a TComponent derivative, then
    deadlocks are more likely when external code is locking and unlocking the monitor in addition to
    the code internal to the class. In many cases, a mutex/critical section/condition variable can be
    created by simply constructing a variable of type TObject and calling the TMonitor.XXXX(ObjInstance)
    methods. }

  PPMonitor = ^PMonitor;
  PMonitor = ^TMonitor;
  TMonitor = record
  strict private
    type
      PWaitingThread = ^TWaitingThread;
      TWaitingThread = record
        Next: PWaitingThread;
        Thread: TThreadID;
        WaitEvent: Pointer;
      end;
      { TSpinWait implements an exponential backoff algorithm for TSpinLock. The algorithm is as follows:
        If the CPUCount > 1, then the first 10 (YieldThreshold) spin cycles (calls to SpinCycle) will use a base 2
        exponentially increasing spin count starting at 4. After 10 cycles, then the behavior reverts to the same
        behavior as when CPUCount = 1.
        If the CPUCount = 1, then it will sleep 1ms every modulus 20 cycles and sleep 0ms every modulus 5 cycles.
        All other cycles simply yield (SwitchToThread - Windows, sched_yield - POSIX). }
      TSpinWait = record
      private const
        YieldThreshold = 10;
        Sleep1Threshold = 20;
        Sleep0Threshold = 5;
      private
        FCount: Integer;
      public
        procedure Reset; inline;
        procedure SpinCycle;
      end;
      { TSpinLock implements a very simple non-reentrant lock. This lock does not block the calling thread using a
        synchronization object. Instead it opts to burn a few extra CPU cycles using the above TSpinWait type. This
        is typically far faster than fully blocking since the length of time the lock is held is relatively few
        cycles and the thread switching overhead will usually far outpace the few cycles burned by simply spin
        waiting. }
      TSpinLock = record
      private
        FLock: Integer;
      public
        procedure Enter;
        procedure Exit;
      end;
    var
      FLockCount: Integer;
      FRecursionCount: Integer;
      FOwningThread: TThreadID;
      FLockEvent: Pointer;
      FSpinCount: Integer;
      FWaitQueue: PWaitingThread;
      FQueueLock: TSpinLock;
    class procedure Spin(Iterations: Integer); static;
    procedure QueueWaiter(var WaitingThread: TWaitingThread);
    procedure RemoveWaiter(var WaitingThread: TWaitingThread);
    function DequeueWaiter: PWaitingThread;
    function GetEvent: Pointer;
    function CheckOwningThread: TThreadID;
    class procedure CheckMonitorSupport; static; inline;
    class function Create: PMonitor; static;
    // Make sure the following Destroy overload is always
    // listed first since it is called from an asm block
    // and there is no overload-resolution done from an
    // assembler symbol reference
  {$IFDEF PUREPASCAL}
  private
  {$ENDIF}
    class procedure Destroy(AObject: TObject); overload; static;
  strict private
    class function GetFieldAddress(AObject: TObject): PPMonitor; inline; static;
    class function GetMonitor(AObject: TObject): PMonitor; static;
    procedure Destroy; overload;
    function Enter(Timeout: Cardinal): Boolean; overload;
    procedure Exit; overload;
    function TryEnter: Boolean; overload;
    function Wait(ALock: PMonitor; Timeout: Cardinal): Boolean; overload;
    procedure Pulse; overload;
    procedure PulseAll; overload;
  public
    { In multi-core/multi-processor systems, it is sometimes desirable to spin for a few cycles instead of blocking
      the current thread when attempting to Enter the monitor. Use SetSpinCount to set a reasonable number of times to
      spin before fully blocking the thread. This value usually obtained through empirical study of the particular
      situation.  }
    class procedure SetSpinCount(AObject: TObject; ASpinCount: Integer); static;
    { Enter locks the monitor object with an optional timeout (in ms) value. Enter without a timeout will wait until
      the lock is obtained. If the procedure returns it can be assumed that the lock was acquired. Enter with a
      timeout will return a boolean status indicating whether or not the lock was obtained (True) or the attempt timed
      out prior to acquire the lock (False). Calling Enter with an INFINITE timeout is the same as calling Enter
      without a timeout.
      TryEnter will simply attempt to obtain the lock and return immediately whether or not the lock was acuired.
      Enter with a 0ms timeout is functionally equivalent to TryEnter.
      Exit will potentially release the lock acquired by a call to Enter or TryEnter. Since Enter/TryEnter are
      rentrant, you must balance each of those calls with a corresponding call to Exit. Only the last call to Exit will
      release the lock and allow other threads to obtain it. Runtime error, reMonitorNoLocked, is generated if Exit is
      called and the calling thread does not own the lock. }
    class procedure Enter(AObject: TObject); overload; static; inline;
    class function Enter(AObject: TObject; Timeout: Cardinal): Boolean; overload; static;
    class procedure Exit(AObject: TObject); overload; static;
    class function TryEnter(AObject: TObject): Boolean; overload; static;
    { Wait will atomically fully release the lock (regardless of the recursion count) and block the calling thread
      until another thread calls Pulse or PulseAll. The first overloaded Wait function will assume the locked object
      and wait object are the same and thus the calling thread must own the lock. The second Wait allows the given
      monitor to atomically unlock the separate monitor lock object and block with the calling thread on the first
      given wait object. Wait will not return (even if it times out) until the monitor lock can be acquired again. It
      is possible for wait to return False (the timeout expired) after a much longer period of time has elapsed if
      the locking object was being held by another thread for an extended period. When Wait returns the recursion
      level of the lock has been restored.
      Pulse must be called on the exact same instance passed to Wait in order to properly release one waiting thread.
      PulseAll works the same as Pulse except that it will release all currently waiting threads.
      Wait/Pulse/PulseAll are the same as a traditional condition variable.
    }
    class function Wait(AObject: TObject; Timeout: Cardinal): Boolean; overload; static;
    class function Wait(AObject, ALock: TObject; Timeout: Cardinal): Boolean; overload; static;
    class procedure Pulse(AObject: TObject); overload; static;
    class procedure PulseAll(AObject: TObject); overload; static;
  end;

const
  INFINITE = Cardinal($FFFFFFFF);       {$EXTERNALSYM INFINITE}

function MonitorEnter(AObject: TObject; Timeout: Cardinal = INFINITE): Boolean; inline;
function MonitorTryEnter(AObject: TObject): Boolean; inline;
procedure MonitorExit(AObject: TObject); inline;
function MonitorWait(AObject: TObject; Timeout: Cardinal): Boolean; inline; overload;
function MonitorWait(AObject, ALock: TObject; Timeout: Cardinal): Boolean; inline; overload;
procedure MonitorPulse(AObject: TObject); inline;
procedure MonitorPulseAll(AObject: TObject); inline;
procedure MemoryBarrier;

procedure YieldProcessor; {$EXTERNALSYM YieldProcessor }

const
  S_OK = 0;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM S_OK}
{$ENDIF}
  S_FALSE = $00000001;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM S_FALSE}
{$ENDIF}
  E_NOINTERFACE = HRESULT($80004002);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM E_NOINTERFACE}
{$ENDIF}
  E_UNEXPECTED = HRESULT($8000FFFF);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM E_UNEXPECTED}
{$ENDIF}
  E_NOTIMPL = HRESULT($80004001);
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM E_NOTIMPL}
{$ENDIF}

type
  IInterface = interface
    ['{00000000-0000-0000-C000-000000000046}']
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;
  {$NODEFINE IInterface}        { defined in sysmac.h }

  IUnknown = IInterface;
  {$EXTERNALSYM IUnknown}       { from unknwn.h or sysmac.h }
{$M+}
  IInvokable = interface(IInterface)
  end;
{$M-}
  {$NODEFINE IInvokable}        { defined in sysmac.h }

  IEnumerator = interface(IInterface)
    function GetCurrent: TObject;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: TObject read GetCurrent;
  end;

  IEnumerable = interface(IInterface)
    function GetEnumerator: IEnumerator;
  end;

  IEnumerator<T> = interface(IEnumerator)
    function GetCurrent: T;
    property Current: T read GetCurrent;
  end;

  IEnumerable<T> = interface(IEnumerable)
    function GetEnumerator: IEnumerator<T>;
  end;

  IComparable = interface(IInterface)
    function CompareTo(Obj: TObject): Integer;
  end;

  IComparable<T> = interface(IComparable)
    function CompareTo(Value: T): Integer;
  end;

  IEquatable<T> = interface(IInterface)
    function Equals(Value: T): Boolean;
  end;

  IDispatch = interface(IUnknown)
    ['{00020400-0000-0000-C000-000000000046}']
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
  end;
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM IDispatch}      { from oaidl.h (oleauto.h) }
{$ENDIF}

{ TInterfacedObject provides a threadsafe default implementation
  of IInterface.  You should use TInterfaceObject as the base class
  of objects implementing interfaces.  }

  TInterfacedObject = class(TObject, IInterface)
  protected
    FRefCount: Integer;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    class function NewInstance: TObject; override;
    property RefCount: Integer read FRefCount;
  end;
  {$NODEFINE TInterfacedObject}         { defined in systobj.h }

  TInterfacedClass = class of TInterfacedObject;

{ TAggregatedObject and TContainedObject are suitable base
  classes for interfaced objects intended to be aggregated
  or contained in an outer controlling object.  When using
  the "implements" syntax on an interface property in
  an outer object class declaration, use these types
  to implement the inner object.

  Interfaces implemented by aggregated objects on behalf of
  the controller should not be distinguishable from other
  interfaces provided by the controller.  Aggregated objects
  must not maintain their own reference count - they must
  have the same lifetime as their controller.  To achieve this,
  aggregated objects reflect the reference count methods
  to the controller.

  TAggregatedObject simply reflects QueryInterface calls to
  its controller.  From such an aggregated object, one can
  obtain any interface that the controller supports, and
  only interfaces that the controller supports.  This is
  useful for implementing a controller class that uses one
  or more internal objects to implement the interfaces declared
  on the controller class.  Aggregation promotes implementation
  sharing across the object hierarchy.

  TAggregatedObject is what most aggregate objects should
  inherit from, especially when used in conjunction with
  the "implements" syntax.  }

  TAggregatedObject = class(TObject)
  private
    FController: Pointer;  // weak reference to controller
    function GetController: IInterface;
  protected
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(const Controller: IInterface);
    property Controller: IInterface read GetController;
  end;
  {$NODEFINE TAggregatedObject} { defined in systobj.h }

  { TContainedObject is an aggregated object that isolates
    QueryInterface on the aggregate from the controller.
    TContainedObject will return only interfaces that the
    contained object itself implements, not interfaces
    that the controller implements.  This is useful for
    implementing nodes that are attached to a controller and
    have the same lifetime as the controller, but whose
    interface identity is separate from the controller.
    You might do this if you don't want the consumers of
    an aggregated interface to have access to other interfaces
    implemented by the controller - forced encapsulation.
    This is a less common case than TAggregatedObject.  }

  TContainedObject = class(TAggregatedObject, IInterface)
  protected
    { IInterface }
    function QueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
  end;
  {$NODEFINE TContainedObject}  { defined in systobj.h }

  TClassHelperBase = class(TInterfacedObject, IInterface)
  protected
    FInstance: TObject;
    constructor _Create(Instance: TObject);
  end;

  TClassHelperBaseClass = class of TClassHelperBase;
  {$NODEFINE TClassHelperBaseClass}
  {$NODEFINE TClassHelperBase}

  { The base class for all custom attributes. Attribute
    instances created by the RTTI unit are owned by those
    members to which they apply. }
  TCustomAttribute = class(TObject)
  end;
  {$NODEFINE TCustomAttribute}

  PShortString = ^ShortString;
  PAnsiString = ^AnsiString;
  PWideString = ^WideString;
  PUnicodeString = ^UnicodeString;
  PString = PUnicodeString;
  {$NODEFINE PShortString}      { defined in sysmac.h }
  {$NODEFINE PAnsiString}       { defined in sysmac.h }
  {$NODEFINE PWideString}       { defined in sysmac.h }
  {$NODEFINE PUnicodeString}    { defined in sysmac.h }
  {$NODEFINE PString}           { defined in sysmac.h }

  UCS2Char = WideChar;
  PUCS2Char = PWideChar;
  UCS4Char = type LongWord;
  {$NODEFINE UCS4Char}          { defined in sysmac.h }
  {-OBJTYPENAME UCS4Char 'BCt'}
  PUCS4Char = ^UCS4Char;
  {$NODEFINE PUCS4Char}         { defined in sysmac.h }

  TUCS4CharArray = array [0..$effffff] of UCS4Char;
  PUCS4CharArray = ^TUCS4CharArray;
  {$NODEFINE TUCS4CharArray}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef UCS4Char* TUCS4CharArray;' *)
  (*$HPPEMIT '}' *)

  UCS4String = array of UCS4Char;
  {$NODEFINE UCS4String}        { defined in sysmac.h }

  UTF8String = type AnsiString(65001);
  PUTF8String = ^UTF8String;

  RawByteString = type AnsiString($ffff);
  PRawByteString = ^RawByteString;

  IntegerArray  = array[0..$effffff] of Integer;
  PIntegerArray = ^IntegerArray;
  {$NODEFINE IntegerArray}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef int* IntegerArray;' *)
  (*$HPPEMIT '}' *)

  {$IF defined(CPUX64)}
  PointerArray = array [0..256*1024*1024 - 2] of Pointer;
  {$ELSE}
  PointerArray = array [0..512*1024*1024 - 2] of Pointer;
  {$IFEND}
  PPointerArray = ^PointerArray;
  {$NODEFINE PointerArray}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef void *PointerArray;' *)
  (*$HPPEMIT '}' *)

  TBoundArray = array of Integer;

  TPCharArray = packed array[0..(MaxLongint div SizeOf(PChar))-1] of PChar;
  PPCharArray = ^TPCharArray;
  {$NODEFINE TPCharArray}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef PChar TPCharArray;' *)
  (*$HPPEMIT '}' *)

  PLongInt      = ^LongInt;
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef int *PLongInt;' *)
  (*$HPPEMIT '  typedef PLongInt PLongint;' *)
  (*$HPPEMIT '}' *)
  {$NODEFINE PLongInt}

  PInteger      = ^Integer;     {$NODEFINE PInteger}    { defined in sysmac.h }
  PCardinal     = ^Cardinal;
  PWord         = ^Word;
  PSmallInt     = ^SmallInt;    {$NODEFINE PSmallInt}   { defined in sysmac.h }
  {$POINTERMATH ON}
  PByte         = ^Byte;        {$NODEFINE PByte}       { defined in sysmac.h }
  {$POINTERMATH OFF}
  PShortInt     = ^ShortInt;    {$NODEFINE PShortInt}   { defined in sysmac.h }
  PInt64        = ^Int64;       {$NODEFINE PInt64}      { defined in sysmac.h }
  PUInt64       = ^UInt64;
  PLongWord     = ^LongWord;    {$NODEFINE PLongWord}   { defined in sysmac.h }
  PSingle       = ^Single;      {$NODEFINE PSingle}     { defined in sysmac.h }
  PDouble       = ^Double;      {$NODEFINE PDouble}     { defined in sysmac.h }
  PDate         = ^Double;
  PDispatch     = ^IDispatch;
  PPDispatch    = ^PDispatch;
  {$NODEFINE PDispatch}  // due to avoid compile error
  {$NODEFINE PPDispatch} // due to avoid compile error
  PError        = ^LongWord;
  PWordBool     = ^WordBool;
  PUnknown      = ^IUnknown;
  PPUnknown     = ^PUnknown;
  PPWideChar    = ^PWideChar;
  PPAnsiChar    = ^PAnsiChar;
  PPChar = PPWideChar;          {$NODEFINE PPChar}      { defined in sysmac.h }
  PExtended     = ^Extended;    {$NODEFINE PExtended}   { defined in sysmac.h }
  PComp         = ^Comp;
  PCurrency     = ^Currency;    {$NODEFINE PCurrency}   { defined in sysmac.h }
  PVariant      = ^Variant;     {$NODEFINE PVariant}    { defined in sysmac.h }
  POleVariant   = ^OleVariant;  {$NODEFINE POleVariant} { defined in sysmac.h }
  PPointer      = ^Pointer;     {$NODEFINE PPointer}    { defined in sysmac.h }
  PBoolean      = ^Boolean;     {$NODEFINE PBoolean}    { defined in sysmac.h }
  PNativeInt    = ^NativeInt;
  PNativeUInt   = ^NativeUInt;

  TDateTime = type Double;
  PDateTime = ^TDateTime;
  {$NODEFINE TDateTime 'TDateTime' 'TDateTimeBase'}     { defined in systdate.h }
  {$OBJTYPENAME TDateTime 'NTDateTime' }

  TDate = type TDateTime;
  TTime = type TDateTime;
  {$NODEFINE TDate 'TDate' 'TDateTimeBase'}
  {$NODEFINE TTime 'TTime' 'TDateTimeBase'}
  {$OBJTYPENAME TDate 'NTDateTime' }
  {$OBJTYPENAME TTime 'NTDateTime' }
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '    typedef System::TDateTime TDate;' *)
  (*$HPPEMIT '    typedef System::TDateTime TTime;' *)
  (*$HPPEMIT '}' *)

  {$IF defined(CPU386)}
  THandle = LongWord;
  {$ELSE}
  THandle = NativeUInt;
  {$IFEND}
  {$NODEFINE THandle}

  TVarArrayBound = packed record
    ElementCount: Integer;
    LowBound: Integer;
  end;
  TVarArrayBoundArray = array [0..0] of TVarArrayBound;
  PVarArrayBoundArray = ^TVarArrayBoundArray;
  TVarArrayCoorArray = array [0..0] of Integer;
  PVarArrayCoorArray = ^TVarArrayCoorArray;

  PVarArray = ^TVarArray;
  TVarArray = packed record
    DimCount: Word;
    Flags: Word;
    ElementSize: Integer;
    LockCount: Integer;
    Data: Pointer;
    Bounds: TVarArrayBoundArray;
  end;

  TVarType = Word;
  PVarData = ^TVarData;
  TVarData = packed record
    case Integer of
      0: (VType: TVarType;
          case Integer of
            0: (Reserved1: Word;
                case Integer of
                  0: (Reserved2, Reserved3: Word;
                      case Integer of
                        varSmallInt: (VSmallInt: SmallInt);
                        varInteger:  (VInteger: Integer);
                        varSingle:   (VSingle: Single);
                        varDouble:   (VDouble: Double);
                        varCurrency: (VCurrency: Currency);
                        varDate:     (VDate: TDateTime);
                        varOleStr:   (VOleStr: PWideChar);
                        varDispatch: (VDispatch: Pointer);
                        varError:    (VError: HRESULT);
                        varBoolean:  (VBoolean: WordBool);
                        varUnknown:  (VUnknown: Pointer);
                        varShortInt: (VShortInt: ShortInt);
                        varByte:     (VByte: Byte);
                        varWord:     (VWord: Word);
                        varLongWord: (VLongWord: LongWord);
                        varInt64:    (VInt64: Int64);
                        varUInt64:   (VUInt64: UInt64);
                        varString:   (VString: Pointer);
                        varAny:      (VAny: Pointer);
                        varArray:    (VArray: PVarArray);
                        varByRef:    (VPointer: Pointer);
                        varUString:  (VUString: Pointer);
                     );
                  1: (VLongs: array[0..2] of LongInt);
               );
            2: (VWords: array [0..6] of Word);
            3: (VBytes: array [0..13] of Byte);
          );
      1: (RawData: array [0..3] of LongInt);
  end;
  {$EXTERNALSYM TVarData}
  {$EXTERNALSYM PVarData}

type
  TVarOp = Integer;

const
  opAdd =        0;
  opSubtract =   1;
  opMultiply =   2;
  opDivide =     3;
  opIntDivide =  4;
  opModulus =    5;
  opShiftLeft =  6;
  opShiftRight = 7;
  opAnd =        8;
  opOr =         9;
  opXor =        10;
  opCompare =    11;
  opNegate =     12;
  opNot =        13;

  opCmpEQ =      14;
  opCmpNE =      15;
  opCmpLT =      16;
  opCmpLE =      17;
  opCmpGT =      18;
  opCmpGE =      19;

  {The number of small block types employed by the default memory manager}
  NumSmallBlockTypes = 55;

type
  { Dispatch call descriptor }
  PCallDesc = ^TCallDesc;
  TCallDesc = packed record
    CallType: Byte;
    ArgCount: Byte;
    NamedArgCount: Byte;
    ArgTypes: array[0..255] of Byte;
  end;

  PDispDesc = ^TDispDesc;
  TDispDesc = packed record
    DispID: Integer;
    ResType: Byte;
    CallDesc: TCallDesc;
  end;

  PVariantManager = ^TVariantManager;
  TVariantManager = record
    VarClear: procedure(var V : Variant);
    VarCopy: procedure(var Dest: Variant; const Source: Variant);
    VarCopyNoInd: procedure; // ARGS PLEASE!
    VarCast: procedure(var Dest: Variant; const Source: Variant; VarType: Integer);
    VarCastOle: procedure(var Dest: Variant; const Source: Variant; VarType: Integer);

    VarToInt: function(const V: Variant): Integer;
    VarToInt64: function(const V: Variant): Int64;
    VarToBool: function(const V: Variant): Boolean;
    VarToReal: function(const V: Variant): Extended;
    VarToCurr: function(const V: Variant): Currency;
    VarToPStr: procedure(var S; const V: Variant);
    VarToLStr: procedure(var S: string; const V: Variant);
    VarToWStr: procedure(var S: WideString; const V: Variant);
    VarToIntf: procedure(var Unknown: IInterface; const V: Variant);
    VarToDisp: procedure(var Dispatch: IDispatch; const V: Variant);
    VarToDynArray: procedure(var DynArray: Pointer; const V: Variant; TypeInfo: Pointer);

    VarFromInt: procedure(var V: Variant; const Value: Integer; const Range: ShortInt);
    VarFromInt64: procedure(var V: Variant; const Value: Int64);
    VarFromBool: procedure(var V: Variant; const Value: Boolean);
    VarFromReal: procedure; // var V: Variant; const Value: Real
    VarFromTDateTime: procedure; // var V: Variant; const Value: TDateTime
    VarFromCurr: procedure; // var V: Variant; const Value: Currency
    VarFromPStr: procedure(var V: Variant; const Value: ShortString);
    VarFromLStr: procedure(var V: Variant; const Value: string);
    VarFromWStr: procedure(var V: Variant; const Value: WideString);
    VarFromIntf: procedure(var V: Variant; const Value: IInterface);
    VarFromDisp: procedure(var V: Variant; const Value: IDispatch);
    VarFromDynArray: procedure(var V: Variant; const DynArray: Pointer; TypeInfo: Pointer);
    OleVarFromPStr: procedure(var V: OleVariant; const Value: ShortString);
    OleVarFromLStr: procedure(var V: OleVariant; const Value: string);
    OleVarFromVar: procedure(var V: OleVariant; const Value: Variant);
    OleVarFromInt: procedure(var V: OleVariant; const Value: Integer; const Range: ShortInt);
    OleVarFromInt64: procedure(var V: OleVariant; const Value: Int64);

    VarOp: procedure(var Left: Variant; const Right: Variant; OpCode: TVarOp);
    VarCmp: procedure(const Left, Right: TVarData; const OpCode: TVarOp); { result is set in the flags }
    VarNeg: procedure(var V: Variant);
    VarNot: procedure(var V: Variant);

    DispInvoke: procedure(Dest: PVarData; const Source: TVarData;
      CallDesc: PCallDesc; Params: Pointer); cdecl;
    VarAddRef: procedure(var V: Variant);

    VarArrayRedim: procedure(var A : Variant; HighBound: Integer);
    VarArrayGet: function(var A: Variant; IndexCount: Integer;
      Indices: Integer): Variant; cdecl;
    VarArrayPut: procedure(var A: Variant; const Value: Variant;
      IndexCount: Integer; Indices: Integer); cdecl;

    WriteVariant: function(var T: Text; const V: Variant; Width: Integer): Pointer;
    Write0Variant: function(var T: Text; const V: Variant): Pointer;
  end deprecated;

  { Dynamic array support }
  PDynArrayTypeInfo = ^TDynArrayTypeInfo;
  {$EXTERNALSYM PDynArrayTypeInfo}
  TDynArrayTypeInfo = packed record
    kind: Byte;
    name: string[0];
    elSize: Longint;
    elType: ^PDynArrayTypeInfo;
    varType: Integer;
  end;
  {$EXTERNALSYM TDynArrayTypeInfo}

  PVarRec = ^TVarRec;
  TVarRec = record { do not pack this record; it is compiler-generated }
    case Integer of
      0: (case Byte of
            vtInteger:       (VInteger: Integer);
            vtBoolean:       (VBoolean: Boolean);
            vtChar:          (VChar: AnsiChar);
            vtExtended:      (VExtended: PExtended);
            vtString:        (VString: PShortString);
            vtPointer:       (VPointer: Pointer);
            vtPChar:         (VPChar: PAnsiChar);
            vtObject:        (VObject: TObject);
            vtClass:         (VClass: TClass);
            vtWideChar:      (VWideChar: WideChar);
            vtPWideChar:     (VPWideChar: PWideChar);
            vtAnsiString:    (VAnsiString: Pointer);
            vtCurrency:      (VCurrency: PCurrency);
            vtVariant:       (VVariant: PVariant);
            vtInterface:     (VInterface: Pointer);
            vtWideString:    (VWideString: Pointer);
            vtInt64:         (VInt64: PInt64);
            vtUnicodeString: (VUnicodeString: Pointer);
         );
      1: (_Reserved1: NativeInt;
          VType:      Byte;
         );
  end;
  {$NODEFINE PVarRec}   { defined in systvar.h }
  {$NODEFINE TVarRec}   { defined in systvar.h }

  {The old memory manager structure (for backward compatibility)}
  PMemoryManager = ^TMemoryManager;
  TMemoryManager = record
    GetMem: function(Size: Integer): Pointer;
    FreeMem: function(P: Pointer): Integer;
    ReallocMem: function(P: Pointer; Size: Integer): Pointer;
  end deprecated 'Use TMemoryManagerEx';

  {The new memory manager structure with expanded functionality}
  PMemoryManagerEx = ^TMemoryManagerEx;
  TMemoryManagerEx = record
    {The basic (required) memory manager functionality}
    GetMem: function(Size: Integer): Pointer;
    FreeMem: function(P: Pointer): Integer;
    ReallocMem: function(P: Pointer; Size: Integer): Pointer;
    {Extended (optional) functionality.}
    AllocMem: function(Size: Cardinal): Pointer;
    RegisterExpectedMemoryLeak: function(P: Pointer): Boolean;
    UnregisterExpectedMemoryLeak: function(P: Pointer): Boolean;
  end;

  THeapStatus = record
    TotalAddrSpace: Cardinal;
    TotalUncommitted: Cardinal;
    TotalCommitted: Cardinal;
    TotalAllocated: Cardinal;
    TotalFree: Cardinal;
    FreeSmall: Cardinal;
    FreeBig: Cardinal;
    Unused: Cardinal;
    Overhead: Cardinal;
    HeapErrorCode: Cardinal;
  end deprecated;

  TSmallBlockTypeState = packed record
    {The internal size of the block type}
    InternalBlockSize: Cardinal;
    {Useable block size: The number of non-reserved bytes inside the block.}
    UseableBlockSize: Cardinal;
    {The number of allocated blocks}
    AllocatedBlockCount: Cardinal;
    {The total address space reserved for this block type (both allocated and
     free blocks)}
    ReservedAddressSpace: Cardinal;
  end;
  TSmallBlockTypeStates = array[0..NumSmallBlockTypes - 1] of TSmallBlockTypeState;

  {The structure returned by GetMemoryManagerState}
  TMemoryManagerState = packed record
    {Small block type states}
    SmallBlockTypeStates: TSmallBlockTypeStates;
    {Medium block stats}
    AllocatedMediumBlockCount: Cardinal;
    TotalAllocatedMediumBlockSize: Cardinal;
    ReservedMediumBlockAddressSpace: Cardinal;
    {Large block stats}
    AllocatedLargeBlockCount: Cardinal;
    TotalAllocatedLargeBlockSize: Cardinal;
    ReservedLargeBlockAddressSpace: Cardinal;
  end;

  PMonitorSupport = ^TMonitorSupport;
  TMonitorSupport = record
    // Obtain a synchronization object - usually an auto-reset event or semaphore
    NewSyncObject: function: Pointer;
    // Free the synchronization object obtained from NewSyncObject
    FreeSyncObject: procedure (SyncObject: Pointer);
    // Obtain a wait object - usually an auto-reset event or semaphore - these should be cached
    NewWaitObject: function: Pointer;
    // Return the wait object from NewWaitObject back to the cache
    FreeWaitObject: procedure (WaitObject: Pointer);
    // Wait for either a SyncObject or WaitObject or signal an object
    // o WaitOrSignalObject(nil, Obj, Timeout); - Wait for <Timeout> time or until <Obj> is signaled
    // o WaitOrSignalObject(Obj, nil, 0); - Signal <Obj> and return. Timeout and WaitObject params ignored.
    WaitOrSignalObject: function (SignalObject, WaitObject: Pointer; Timeout: Cardinal): Cardinal;
  end;

  {Memory map}
  TChunkStatus = (csUnallocated, csAllocated, csReserved,
    csSysAllocated, csSysReserved);
  TMemoryMap = array[0..65535] of TChunkStatus;

  {Block alignment options}
  TMinimumBlockAlignment = (mba8Byte, mba16Byte);

{$IFDEF PC_MAPPED_EXCEPTIONS}
  PUnwinder = ^TUnwinder;
  TUnwinder = record
    RaiseException: function(Exc: Pointer): LongBool; cdecl;
    RegisterIPLookup: function(fn: Pointer; StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool; cdecl;
    UnregisterIPLookup: procedure(StartAddr: LongInt) cdecl;
    DelphiLookup: function(Addr: LongInt; Context: Pointer): Pointer; cdecl;
    ClosestHandler: function(Context: Pointer): LongWord; cdecl;
  end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

  PackageUnitEntry = packed record
    Init, FInit : Pointer;
  end;

  { Compiler generated table to be processed sequentially to init & finit all package units }
  { Init: 0..Max-1; Final: Last Initialized..0                                              }
  UnitEntryTable = array [0..9999999] of PackageUnitEntry;
  PUnitEntryTable = ^UnitEntryTable;
  { Pointer in this table is PPTypeInfo, except when it's not; if the value is 1,
    then it's a "unit boundary" marker, indicating that following types are in
    the next unit along in the TPackageTypeInfo.UnitNames unit name list sequence. }
  TTypeTable = array[0..MaxInt div SizeOf(Pointer) - 1] of Pointer;
  PTypeTable = ^TTypeTable;

  {$NODEFINE UnitEntryTable}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '    typedef struct PackageUnitEntry UnitEntryTable;' *)
  (*$HPPEMIT '}' *)

  PPackageTypeInfo = ^TPackageTypeInfo;
  TPackageTypeInfo = record
    TypeCount: Integer;
    TypeTable: PTypeTable;
    UnitCount: Integer;
    UnitNames: PShortString; { concatenation of Pascal strings, one for each unit }
  end;

  PackageInfoTable = record
    UnitCount: Integer;      { number of entries in UnitInfo array; always > 0 }
    UnitInfo: PUnitEntryTable;
    TypeInfo: TPackageTypeInfo;
  end;

  PackageInfo = ^PackageInfoTable;

  { Each package exports a '@GetPackageInfoTable' which can be used to retrieve }
  { the table which contains compiler generated information about the package DLL }
  GetPackageInfoTable = function : PackageInfo;

{$IFDEF DEBUG_FUNCTIONS}
{ Inspector Query; implementation in GETMEM.INC; no need to conditionalize that }
  THeapBlock = record
    Start: Pointer;
    Size: Cardinal;
  end;

  THeapBlockArray = array of THeapBlock;
  TObjectArray = array of TObject;

function GetHeapBlocks: THeapBlockArray;
function FindObjects(AClass: TClass; FindDerived: Boolean): TObjectArray;
{ Inspector Query }
{$ENDIF}

{
  When an exception is thrown, the exception object that is thrown is destroyed
  automatically when the except clause which handles the exception is exited.
  There are some cases in which an application may wish to acquire the thrown
  object and keep it alive after the except clause is exited.  For this purpose,
  we have added the AcquireExceptionObject and ReleaseExceptionObject functions.
  These functions maintain a reference count on the most current exception object,
  allowing applications to legitimately obtain references.  If the reference count
  for an exception that is being thrown is positive when the except clause is exited,
  then the thrown object is not destroyed by the RTL, but assumed to be in control
  of the application.  It is then the application's responsibility to destroy the
  thrown object.  If the reference count is zero, then the RTL will destroy the
  thrown object when the except clause is exited.
}
function AcquireExceptionObject: Pointer;
procedure ReleaseExceptionObject;

{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure GetUnwinder(var Dest: TUnwinder);
procedure SetUnwinder(const NewUnwinder: TUnwinder);
function IsUnwinderSet: Boolean;

//function SysRegisterIPLookup(ModuleHandle, StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool;
{
  Do NOT call these functions.  They are for internal use only:
    SysRegisterIPLookup
    SysUnregisterIPLookup
    BlockOSExceptions
    UnblockOSExceptions
    AreOSExceptionsBlocked
}
function SysRegisterIPLookup(StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool;
procedure SysUnregisterIPLookup(StartAddr: LongInt);
//function SysAddressIsInPCMap(Addr: LongInt): Boolean;
function SysClosestDelphiHandler(Context: Pointer): LongWord;
procedure BlockOSExceptions;
procedure UnblockOSExceptions;
function AreOSExceptionsBlocked: Boolean;

{$ELSE !PC_MAPPED_EXCEPTIONS}
// These functions are not portable.  Use AcquireExceptionObject above instead
function RaiseList: Pointer; deprecated 'Use AcquireExceptionObject';  { Stack of current exception objects }
function SetRaiseList(NewPtr: Pointer): Pointer; deprecated 'Use AcquireExceptionObject';  { returns previous value }
{$ENDIF !PC_MAPPED_EXCEPTIONS}

function ExceptObject: TObject;
function ExceptAddr: Pointer;

{
  Coverage support.  These are internal use structures referenced by compiler
  helper functions for QA coverage support.
}
type
    TCVModInfo = packed record
        ModName: PAnsiChar;
        LibName: PAnsiChar;
        UserData: Pointer;
        end;
    PCVModInfo = ^TCVModInfo;

{$EXTERNALSYM _CVR_PROBE}
procedure _CVR_PROBE(mi: PCVModInfo; probeNum: Cardinal); cdecl;
{$EXTERNALSYM _CVR_STMTPROBE}
function _CVR_STMTPROBE(mi: PCVModInfo; probeNum: Cardinal; TrueFalse: Cardinal): Boolean; cdecl;

type
  TAssertErrorProc = procedure (const Message, Filename: string;
    LineNumber: Integer; ErrorAddr: Pointer);
  TSafeCallErrorProc = procedure (ErrorCode: HResult; ErrorAddr: Pointer);
  TRaiseExceptionProc = procedure (ExceptionCode, ExceptionFlags: LongWord;
    NumberOfArguments: LongWord; Args: Pointer); stdcall;

{$IFDEF DEBUG}
{
  This variable is just for debugging the exception handling system.  See
  _DbgExcNotify for the usage.
}
var
  ExcNotificationProc : procedure(NotificationKind: Integer;
                                  ExceptionObject: Pointer;
                                  ExceptionName: PShortString;
                                  ExceptionLocation: Pointer;
                                  HandlerAddr: Pointer) = nil;
{$ENDIF DEBUG}

var
  DispCallByIDProc: Pointer;
  ExceptProc: Pointer;    { Unhandled exception handler }
  ErrorProc: procedure (ErrorCode: Byte; ErrorAddr: Pointer);     { Error handler procedure }
{$IFDEF MSWINDOWS}
  ExceptClsProc: Pointer; { Map an OS Exception to a Delphi class reference }
  ExceptObjProc: Pointer; { Map an OS Exception to a Delphi class instance }
{$IF defined(CPU386)}
  RaiseExceptionProc: Pointer;
{$ELSE}
  RaiseExceptionProc: TRaiseExceptionProc;
{$IFEND}
  RTLUnwindProc: Pointer;
{$ENDIF MSWINDOWS}
  RaiseExceptObjProc: Pointer; { notify of the raise of an exception object }
  ExceptionAcquired: Pointer; { notification that a given exception object has been "acquired" (C++)}
  ExceptionClass: TClass; { Exception base class (must be Exception) }
  SafeCallErrorProc: TSafeCallErrorProc; { Safecall error handler }
  AssertErrorProc: TAssertErrorProc; { Assertion error handler }
  ExitProcessProc: procedure; { Hook to be called just before the process actually exits }
  AbstractErrorProc: procedure; { Abstract method error handler }
  HPrevInst: LongWord deprecated;    { Handle of previous instance - HPrevInst cannot be tested for multiple instances in Win32}
{$IF defined(CPUX64)}
  MainInstance: THandle;    { Handle of the main(.EXE) HInstance }
{$ELSE}
  MainInstance: LongWord;   { Handle of the main(.EXE) HInstance }
{$IFEND}
{$IFDEF MSWINDOWS}
  {$NODEFINE MainInstance}
  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '    extern PACKAGE HINSTANCE MainInstance;' *)
  (*$HPPEMIT '}' *)
{$ENDIF}
  MainThreadID: TThreadID;  { ThreadID of thread that module was initialized in }
  IsLibrary: Boolean;       { True if module is a DLL }
{$IFDEF MSWINDOWS}
  CmdShow: Integer platform;{ CmdShow parameter for CreateWindow }
  CmdLine: PChar platform;  { Command line pointer }
{$ENDIF MSWINDOWS}
  InitProc: Pointer;        { Last installed initialization procedure }
  ExitCode: Integer = 0;    { Program result }
  ExitProc: Pointer;        { Last installed exit procedure }
  ErrorAddr: Pointer = nil; { Address of run-time error }
  RandSeed: Longint = 0;    { Base for random number generator }
  IsConsole: Boolean;       { True if compiled as console app }
  IsMultiThread: Boolean;   { True if more than one thread }
  FileMode: Byte = 2;       { Standard mode for opening files }
{$IF defined(LINUX) or defined(MACOS)}
  FileAccessRights: Integer platform; { Default access rights for opening files }
  ArgCount: Integer platform;
  ArgValues: PPAnsiChar platform;
  envp: PPChar platform;
{$IFEND LINUX or MACOS}
  Test8086: Byte;           { CPU family (minus one) See consts below }
  Test8087: Byte = 3;       { assume 80387 FPU or OS supplied FPU emulation }
  TestFDIV: Shortint;       { -1: Flawed Pentium, 0: Not determined, 1: Ok }
  CPUCount: Integer;        { Number of CPU Cores detected }
  Input: Text;              { Standard input }
  Output: Text;             { Standard output }
  ErrOutput: Text;          { Standard error output }

  VarClearProc:  procedure (var v: TVarData) = nil; // for internal use only
  VarAddRefProc: procedure (var v: TVarData) = nil; // for internal use only
  VarCopyProc:   procedure (var Dest: TVarData; const Source: TVarData) = nil; // for internal use only
  VarToLStrProc: procedure (var Dest: AnsiString; const Source: TVarData) = nil;   // for internal use only
  VarToWStrProc: procedure (var Dest: WideString; const Source: TVarData) = nil;   // for internal use only

  MonitorSupport: PMonitorSupport;
  {$EXTERNALSYM MonitorSupport}

const
  CPUi386     = 2;
  CPUi486     = 3;
  CPUPentium  = 4;

var
  Default8087CW: Word = $1332;{ Default 8087 control word.  FPU control
                                register is set to this value.
                                CAUTION:  Setting this to an invalid value
                                could cause unpredictable behavior. }

  HeapAllocFlags: Word platform = 2;   { Heap allocation flags, gmem_Moveable }
  DebugHook: Byte platform = 0;        { 1 to notify debugger of non-Delphi exceptions
                                >1 to notify debugger of exception unwinding }
  JITEnable: Byte platform = 0;        { 1 to call UnhandledExceptionFilter if the exception
                                is not a Pascal exception.
                                >1 to call UnhandledExceptionFilter for all exceptions }
  NoErrMsg: Boolean platform = False;  { True causes the base RTL to not display the message box
                                when a run-time error occurs }
{$IF defined(LINUX) or defined(MACOS)}
                              { CoreDumpEnabled = True will cause unhandled
                                exceptions and runtime errors to raise a
                                SIGABRT signal, which will cause the OS to
                                coredump the process address space.  This can
                                be useful for postmortem debugging. }
  CoreDumpEnabled: Boolean platform = False;
{$IFEND LINUX or MACOS}
  DefaultSystemCodePage: Integer;
  DefaultUnicodeCodePage: Integer; { Used by _NewUnicodeString to set the codePage field of strRec }
{$IFDEF MSWINDOWS}
  UTF8CompareLocale: Cardinal;
{$ENDIF MSWINDOWS}

{$IFDEF POSIX}
  function UTF8CompareLocale: Pointer;
  function SetUTF8CompareLocale(const LocaleName: string): Boolean; platform;
{$ENDIF}

type
  TTextLineBreakStyle = (tlbsLF, tlbsCRLF);

var   { Text output line break handling.  Default value for all text files }
  DefaultTextLineBreakStyle: TTextLineBreakStyle = {$IFDEF LINUX} tlbsLF {$ENDIF}
                                                 {$IFDEF MSWINDOWS} tlbsCRLF {$ENDIF}
                                                 {$IFDEF MACOS} tlbsLF {$ENDIF};
const
   sLineBreak = {$IFDEF LINUX} AnsiChar(#10) {$ENDIF}
      {$IFDEF MSWINDOWS} AnsiString(#13#10) {$ENDIF}
      {$IFDEF MACOS} AnsiChar(#10) {$ENDIF};

type
  HRSRC = THandle;              { from windef.h / winnt.h }
  TResourceHandle = HRSRC;   // make an opaque handle type
  HINST = THandle;              { HINSTANCE from widnef.h }
  HMODULE = HINST;              { from windef.h }
  HGLOBAL = THandle;            { from windef.h }
{$IFDEF MSWINDOWS}
  {$EXTERNALSYM HRSRC}
  {$NODEFINE HINST}
  {$EXTERNALSYM HMODULE}
  {$EXTERNALSYM HGLOBAL}
{$ENDIF}


{$IFDEF POSIX}
{ ELF resources }
function FindResource(ModuleHandle: HMODULE; ResourceName, ResourceType: PChar): TResourceHandle;
function LoadResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): HGLOBAL;
function SizeofResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): Integer;
function LockResource(ResData: HGLOBAL): Pointer;
function UnlockResource(ResData: HGLOBAL): LongBool;
function FreeResource(ResData: HGLOBAL): LongBool;
{$ENDIF POSIX}

{ Memory manager support }

procedure GetMemoryManager(var MemMgr: TMemoryManager); overload; deprecated;
procedure SetMemoryManager(const MemMgr: TMemoryManager); overload; deprecated;
procedure GetMemoryManager(var MemMgrEx: TMemoryManagerEx); overload;
procedure SetMemoryManager(const MemMgrEx: TMemoryManagerEx); overload;
function IsMemoryManagerSet: Boolean;

function SysGetMem(Size: Integer): Pointer;
function SysFreeMem(P: Pointer): Integer;
function SysReallocMem(P: Pointer; Size: Integer): Pointer;
function SysAllocMem(Size: Cardinal): Pointer;
function SysRegisterExpectedMemoryLeak(P: Pointer): Boolean;
function SysUnregisterExpectedMemoryLeak(P: Pointer): Boolean;

{ AllocMem allocates a block of the given size on the heap. Each byte in
  the allocated buffer is set to zero. To dispose the buffer, use the
  FreeMem standard procedure. }

function AllocMem(Size: Cardinal): Pointer;

var

  AllocMemCount: Integer deprecated; {Unsupported}
  AllocMemSize: Integer deprecated; {Unsupported}

{Set this variable to true to report memory leaks on shutdown. This setting
 has no effect if this module is sharing a memory manager owned by another
 module.}
  ReportMemoryLeaksOnShutdown: Boolean;

{Set this variable to true to employ a "busy waiting" loop instead of putting
 the thread to sleep if a thread contention occurs inside the memory manager.
 This may improve performance on multi-CPU systems with a relatively low thread
 count, but will hurt performance otherwise.}
  NeverSleepOnMMThreadContention: Boolean;

{$IFDEF MSWINDOWS}
function GetHeapStatus: THeapStatus; platform; deprecated; {Unsupported}

{Returns information about the current state of the memory manager}
procedure GetMemoryManagerState(var AMemoryManagerState: TMemoryManagerState);

{Gets the state of every 64K block in the 4GB address space}
procedure GetMemoryMap(var AMemoryMap: TMemoryMap);

{Registers expected memory leaks. Returns true on success. The list of leaked
 blocks is limited in size, so failure is possible if the list is full.}
function RegisterExpectedMemoryLeak(P: Pointer): boolean;

{Removes expected memory leaks. Returns true if the previously registered leak
 was found and removed.}
function UnregisterExpectedMemoryLeak(P: Pointer): boolean;

{Set the minimum block alignment. In the current implementation blocks >=160
 bytes will always be at least 16 byte aligned, even if only 8-byte alignment
 (the default) is required.}
function GetMinimumBlockAlignment: TMinimumBlockAlignment;
procedure SetMinimumBlockAlignment(AMinimumBlockAlignment: TMinimumBlockAlignment);

{Searches the current process for a shared memory manager. If no memory has
 been allocated using this memory manager it will switch to using the shared
 memory manager instead. Returns true if another memory manager was found and
 this module is now sharing it.}
function AttemptToUseSharedMemoryManager: Boolean;

{Makes this memory manager available for sharing to other modules in the
 current process. Only one memory manager may be shared per process, so this
 function may fail.}
function ShareMemoryManager: Boolean;

{$ENDIF}

{ Thread support }
type
  TThreadFunc = function(Parameter: Pointer): Integer;

{$IFDEF POSIX}

{$IFDEF LINUX}
type
  TSize_T = Cardinal;

  TSchedParam = record
    sched_priority: Integer;
  end;
  {$DEFINE _PTHREAD_ATTR_T_DEFINED}
  pthread_attr_t = record
    __detachstate,
    __schedpolicy: Integer;
    __schedparam: TSchedParam;
    __inheritsched,
    __scope: Integer;
    __guardsize: TSize_T;
    __stackaddr_set: Integer;
    __stackaddr: Pointer;
    __stacksize: TSize_T;
  end;
  {$EXTERNALSYM pthread_attr_t}
{$ENDIF LINUX}
{$IFDEF MACOS}
const
   PTHREAD_ATTR_SIZE = 36;
   SCHED_PARAM_SIZE = 4;
type
  TSchedParam = record
    sched_priority: Integer;
    opaque: array [0..SCHED_PARAM_SIZE] of Byte;
  end;
  {$DEFINE _PTHREAD_ATTR_T_DEFINED}
   pthread_attr_t = record
      __sig: Longint;
      opaque: array [0..PTHREAD_ATTR_SIZE] of Byte;
   end;
  {$EXTERNALSYM pthread_attr_t}  // Defined in signal.h
{$ENDIF MAXOSX}

type
  TThreadAttr = pthread_attr_t;
  PThreadAttr = ^TThreadAttr;

  TBeginThreadProc = function (Attribute: PThreadAttr;
    ThreadFunc: TThreadFunc; Parameter: Pointer;
    var ThreadId: TThreadID): Integer;
  TEndThreadProc = procedure(ExitCode: Integer);

var
  BeginThreadProc: TBeginThreadProc = nil;
  EndThreadProc: TEndThreadProc = nil;
{$ENDIF POSIX}

{$IFDEF MSWINDOWS}

type
  TSystemThreadFuncProc = function(ThreadFunc: TThreadFunc; Parameter: Pointer): Pointer;
  TSystemThreadEndProc = procedure(ExitCode: Integer);
  {$NODEFINE TSystemThreadFuncProc}
  {$NODEFINE TSystemThreadEndProc}

  (*$HPPEMIT 'namespace System' *)
  (*$HPPEMIT '{' *)
  (*$HPPEMIT '  typedef void * (__fastcall * TSystemThreadFuncProc)(void *, void * );' *)
  (*$HPPEMIT '  typedef void (__fastcall * TSystemThreadEndProc)(int);' *)
  (*$HPPEMIT '}' *)

var
  // SystemThreadFuncProc and SystemThreadEndProc are set during the startup
  // code by the C++ RTL when running in a C++Builder VCL application.
  SystemThreadFuncProc: TSystemThreadFuncProc = nil;
  SystemThreadEndProc: TSystemThreadEndProc = nil;

function BeginThread(SecurityAttributes: Pointer; StackSize: LongWord;
  ThreadFunc: TThreadFunc; Parameter: Pointer; CreationFlags: LongWord;
  var ThreadId: TThreadID): Integer;
{$ENDIF}
{$IFDEF POSIX}
function BeginThread(Attribute: PThreadAttr; ThreadFunc: TThreadFunc;
                     Parameter: Pointer; var ThreadId: TThreadID): Integer;

{$ENDIF}
procedure EndThread(ExitCode: Integer);

{ Standard procedures and functions }

const
{ File mode magic numbers }

  fmClosed = $D7B0;
  fmInput  = $D7B1;
  fmOutput = $D7B2;
  fmInOut  = $D7B3;

{ Text file flags         }
  tfCRLF   = $1;    // Dos compatibility flag, for CR+LF line breaks and EOF checks

type
{ Typed-file and untyped-file record }

  TFileRec = packed record (* must match the size the compiler generates: 592 bytes (616 bytes for x64) *)
{$IF defined(CPUX64)}
    Handle: THandle;
{$ELSE}
    Handle: Integer;
{$IFEND}
    Mode: Word;
    Flags: Word;
    case Byte of
      0: (RecSize: Cardinal);   //  files of record
      1: (BufSize: Cardinal;    //  text files
          BufPos: Cardinal;
          BufEnd: Cardinal;
          BufPtr: PAnsiChar;
          OpenFunc: Pointer;
          InOutFunc: Pointer;
          FlushFunc: Pointer;
          CloseFunc: Pointer;
          UserData: array[1..32] of Byte;
          Name: array[0..259] of WideChar;
      );
  end;

{ Text file record structure used for Text files }
  PTextBuf = ^TTextBuf;
  TTextBuf = array[0..127] of AnsiChar;
  TTextRec = packed record (* must match the size the compiler generates: 720 bytes (744 bytes for x64) *)
{$IF defined(CPUX64)}
    Handle: THandle;       (* must overlay with TFileRec *)
{$ELSE}
    Handle: Integer;       (* must overlay with TFileRec *)
{$IFEND}
    Mode: Word;
    Flags: Word;
    BufSize: Cardinal;
    BufPos: Cardinal;
    BufEnd: Cardinal;
    BufPtr: PAnsiChar;
    OpenFunc: Pointer;
    InOutFunc: Pointer;
    FlushFunc: Pointer;
    CloseFunc: Pointer;
    UserData: array[1..32] of Byte;
    Name: array[0..259] of WideChar;
    Buffer: TTextBuf;
  end;

  TTextIOFunc = function (var F: TTextRec): Integer;
  TFileIOFunc = function (var F: TFileRec): Integer;

procedure SetLineBreakStyle(var T: Text; Style: TTextLineBreakStyle);
procedure __IOTest;
procedure SetInOutRes(NewValue: Integer);
procedure ChDir(const S: string); overload;
procedure ChDir(P: PChar); overload;
function Flush(var t: Text): Integer;
procedure _UGetDir(D: Byte; var S: UnicodeString);
procedure _LGetDir(D: Byte; var S: AnsiString);
procedure _WGetDir(D: Byte; var S: WideString);
procedure _SGetDir(D: Byte; var S: ShortString);
function IOResult: Integer;
procedure MkDir(const S: string); overload;
procedure MkDir(P: PChar); overload;
procedure Move(const Source; var Dest; Count: Integer);
procedure MoveChars(const Source; var Dest; Length: Integer); inline;
function ParamCount: Integer;
function ParamStr(Index: Integer): string;
procedure RmDir(const S: string); overload;
procedure RmDir(P: PChar); overload;
function UpCase(Ch: AnsiChar): AnsiChar; overload; inline;
function UpCase(Ch: WideChar): WideChar; overload; inline;

{ random functions }
procedure Randomize;

function Random(const ARange: Integer): Integer; overload;
function Random: Extended; overload;


{ Control 8087 control word }

procedure Reset8087CW; // Resets to Default8087CW
procedure Set8087CW(NewCW: Word);
function Get8087CW: Word;

{ Wide character support procedures and functions for C++ }
{ These functions should not be used in Delphi code!
 (conversion is implicit in Delphi code)      }

function WideCharToString(Source: PWideChar): UnicodeString;
function WideCharLenToString(Source: PWideChar; SourceLen: Integer): UnicodeString;
procedure WideCharToStrVar(Source: PWideChar; var Dest: UnicodeString);
procedure WideCharLenToStrVar(Source: PWideChar; SourceLen: Integer;
  var Dest: UnicodeString); overload;
procedure WideCharLenToStrVar(Source: PWideChar; SourceLen: Integer;
  var Dest: AnsiString); overload;
function StringToWideChar(const Source: UnicodeString; Dest: PWideChar;
  DestSize: Integer): PWideChar;

{ PUCS4Chars returns a pointer to the UCS4 char data in the
  UCS4String array, or a pointer to a null char if UCS4String is empty }

function PUCS4Chars(const S: UCS4String): PUCS4Char;

{ Widestring <-> UCS4 conversion }

function WideStringToUCS4String(const S: WideString): UCS4String;
function UCS4StringToWideString(const S: UCS4String): WideString;

{ PAnsiChar/PWideChar Unicode <-> UTF8 conversion }

// UnicodeToUTF8(3):
// UTF8ToUnicode(3):
// Scans the source data to find the null terminator, up to MaxBytes
// Dest must have MaxBytes available in Dest.
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.

function UnicodeToUtf8(Dest: PAnsiChar; Source: PWideChar; MaxBytes: Integer): Integer; overload; deprecated;
function Utf8ToUnicode(Dest: PWideChar; Source: PAnsiChar; MaxChars: Integer): Integer; overload; deprecated;

// UnicodeToUtf8(4):
// UTF8ToUnicode(4):
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.
// Nulls in the source data are not considered terminators - SourceChars must be accurate

function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal; overload;
function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PAnsiChar; SourceBytes: Cardinal): Cardinal; overload;

{ WideString <-> UTF8 conversion }

function UTF8Encode(const WS: WideString): RawByteString; overload;
function UTF8Encode(const US: UnicodeString): RawByteString; overload;
function UTF8Encode(const A: RawByteString): RawByteString; overload;
function UTF8EncodeToShortString(const WS: WideString): ShortString; overload;
function UTF8EncodeToShortString(const US: UnicodeString): ShortString; overload;
function UTF8EncodeToShortString(const A: RawByteString): ShortString; overload;
function UTF8Decode(const S: RawByteString): WideString; deprecated 'Use UTF8ToWideString or UTF8ToString';
function UTF8ToWideString(const S: RawByteString): WideString; inline;
function UTF8ToUnicodeString(const S: RawByteString): UnicodeString; overload;
function UTF8ToUnicodeString(const S: ShortString): UnicodeString; overload;
function UTF8ToUnicodeString(const S: PAnsiChar): UnicodeString; overload;
function UTF8ToString(const S: RawByteString): string; inline; overload;
function UTF8ToString(const S: ShortString): string; inline; overload;
function UTF8ToString(const S: PAnsiChar): string; inline; overload;
function UTF8ToString(const S: array of AnsiChar): string; overload;
//function UTF8ToUnicodeString(const S: ShortString): UnicodeString; overload;

{ Ansi <-> UTF8 conversion }


function AnsiToUtf8(const S: string): RawByteString;
function Utf8ToAnsi(const S: RawByteString): string;

{ OLE string support procedures and functions }

function OleStrToString(Source: PWideChar): UnicodeString;
procedure OleStrToStrVar(Source: PWideChar; var Dest: AnsiString); overload;
procedure OleStrToStrVar(Source: PWideChar; var Dest: UnicodeString); overload;
function StringToOleStr(const Source: AnsiString): PWideChar; overload;
function StringToOleStr(const Source: UnicodeString): PWideChar; overload;

{ Variant manager support procedures and functions (obsolete - see Variants.pas) }

procedure GetVariantManager(var VarMgr: TVariantManager); deprecated;
procedure SetVariantManager(const VarMgr: TVariantManager); deprecated;
function IsVariantManagerSet: Boolean; deprecated;

{ Interface dispatch support }

{$IF defined(CPU386)}
procedure _IntfDispCall; cdecl; // ARGS PLEASE!
procedure _IntfVarCall; cdecl; // ARGS PLEASE!
{$ELSE}
procedure _IntfDispCall(Result: Pointer; const Dispatch: IDispatch;
  DispDesc: PDispDesc; Params: Pointer); cdecl;
procedure _IntfVarCall(Dest: PVarData; const Source: TVarData;
  CallDesc: PCallDesc; Params: Pointer); cdecl;
{$IFEND}

{ Dynamic method dispatch support }

function GetDynaMethod(vmt: TClass; selector: Smallint): Pointer;

{ Package/Module registration and unregistration }

type
  PLibModule = ^TLibModule;
  TLibModule = record
    Next: PLibModule;
{$IF defined(CPU386)}
    Instance: LongWord;
    CodeInstance: LongWord;
    DataInstance: LongWord;
    ResInstance: LongWord;
{$ELSE}
    Instance: HINST;
    CodeInstance: HINST;
    DataInstance: HINST;
    ResInstance: HINST;
{$IFEND}
    TypeInfo: PPackageTypeInfo;
    Reserved: Integer;
{$IF defined(LINUX) or defined(MACOS)}
    InstanceVar: Pointer platform;
    InitTable: Pointer platform;
    GOT: LongWord platform;
{$IFEND LINUX or MACOS}
{$IFDEF PC_MAPPED_EXCEPTIONS}
    CodeSegStart: LongWord platform;
    CodeSegEnd: LongWord platform;
{$ENDIF PC_MAPPED_EXCEPTIONS}
  end;

{$IF defined(CPU386)}
  TEnumModuleFunc = function (HInstance: Integer; Data: Pointer): Boolean;
  TEnumModuleFuncLW = function (HInstance: LongWord; Data: Pointer): Boolean;
  TModuleUnloadProc = procedure (HInstance: Integer);
  TModuleUnloadProcLW = procedure (HInstance: LongWord);
{$ELSE}
  TEnumModuleFunc = function (HInstance: NativeInt; Data: Pointer): Boolean;
  TEnumModuleFuncLW = function (HInstance: THandle; Data: Pointer): Boolean;
  TModuleUnloadProc = procedure (HInstance: NativeInt);
  TModuleUnloadProcLW = procedure (HInstance: THandle);
{$IFEND}

  PModuleUnloadRec = ^TModuleUnloadRec;
  TModuleUnloadRec = record
    Next: PModuleUnloadRec;
    Proc: TModuleUnloadProcLW;
  end;

var
  LibModuleList: PLibModule = nil;
  ModuleUnloadList: PModuleUnloadRec = nil;

procedure RegisterModule(LibModule: PLibModule);
procedure UnregisterModule(LibModule: PLibModule);
{$IF defined(CPU386)}
function FindHInstance(Address: Pointer): LongWord;
function FindClassHInstance(ClassType: TClass): LongWord;
{$ELSE}
function FindHInstance(Address: Pointer): HINST;
function FindClassHInstance(ClassType: TClass): HINST;
{$IFEND}
{$IF defined(CPU386)}
function FindResourceHInstance(Instance: LongWord): LongWord;
{$ELSE}
function FindResourceHInstance(Instance: HINST): HINST;
{$IFEND}
{$IFDEF MSWINDOWS}
function GetResourceModuleName(HostAppName, ModuleName: string): string;
{$ENDIF}
function LoadResourceModule(ModuleName: PChar; CheckOwner: Boolean = True): LongWord;
procedure EnumModules(Func: TEnumModuleFunc; Data: Pointer); overload;
procedure EnumResourceModules(Func: TEnumModuleFunc; Data: Pointer); overload;
procedure EnumModules(Func: TEnumModuleFuncLW; Data: Pointer); overload;
procedure EnumResourceModules(Func: TEnumModuleFuncLW; Data: Pointer); overload;
procedure AddModuleUnloadProc(Proc: TModuleUnloadProc); overload;
procedure RemoveModuleUnloadProc(Proc: TModuleUnloadProc); overload;
procedure AddModuleUnloadProc(Proc: TModuleUnloadProcLW); overload;
procedure RemoveModuleUnloadProc(Proc: TModuleUnloadProcLW); overload;
{$IF defined(LINUX) or defined(MACOS)}
{ Given an HMODULE, this function will return its fully qualified name.  There is
  no direct equivalent in Linux so this function provides that capability. }
function GetModuleFileName(Module: HMODULE; Buffer: PChar; BufLen: Integer): Integer;
{$IFEND LINUX or MACOS}

{ ResString support function/record }

type
  PResStringRec = ^TResStringRec;
{$IF defined(CPU386)}
  // 32bit = 8 bytes
  TResStringRec = packed record
    Module: ^Cardinal;
    Identifier: Integer;
  end;
{$ELSE}
  // 64bit = 16 bytes
  TResStringRec = record
    Module: ^HMODULE;
    Identifier: LongInt;
  end;
{$IFEND}

function LoadResString(ResStringRec: PResStringRec): string;

function Int(const X: Extended): Extended;
function Frac(const X: Extended): Extended;
function Exp(const X: Extended): Extended;
function Cos(const X: Extended): Extended;
function Sin(const X: Extended): Extended;
function Ln(const X: Extended): Extended;
function ArcTan(const X: Extended): Extended;
function Sqrt(const X: Extended): Extended;

{ Procedures and functions that need compiler magic }

{$IF defined(CPU386)}
procedure _ROUND;
procedure _TRUNC;
{$ELSE}
function _ROUND(val: Extended): Int64;
function _TRUNC(val: Extended): Int64;
{$IFEND}

procedure _AbstractError;
procedure _Assert(const Message, Filename: string; LineNumber: Integer);
function _Append(var t: TTextRec): Integer;
function _Assign(var t: TTextRec; const s: PChar): Integer;
function _BlockRead(var f: TFileRec; buffer: Pointer; recCnt: Longint; var recsRead: Longint): Longint;
function _BlockWrite(var f: TFileRec; buffer: Pointer; recCnt: Longint; var recsWritten: Longint): Longint;
function _Close(var t: TTextRec): Integer;
function _EofFile(var f: TFileRec): Boolean;
function _EofText(var t: TTextRec): Boolean;
function _Eoln(var t: TTextRec): Boolean;
procedure _Erase(var f: TFileRec);
{$IFDEF TRIAL_EDITION}
procedure _Expired;
{$ENDIF}
function _FilePos(var f: TFileRec): Longint;
function _FileSize(var f: TFileRec): Longint;
function _Flush(var t: TTextRec): Integer;
procedure _FillChar(var Dest; Count: Integer; Value: AnsiChar);
function _FreeMem(P: Pointer): Integer;
function _GetMem(Size: Integer): Pointer;
function _ReallocMem(var P: Pointer; NewSize: Integer): Pointer;
procedure _Halt(Code: Integer);
procedure _Halt0;
{$IFDEF TRIAL_EDITION}
{$IFDEF MSWINDOWS}
function _InitUnitPrep: Int64;
{$ENDIF}
{$IFDEF LINUX}
function _InitUnitPrep: Integer;
{$ENDIF}
{$ENDIF}
procedure Mark; deprecated;
function _ReadRec(var f: TFileRec; Buffer: Pointer): Integer;
{$IF not defined(CPU386)}
function _ReadCharEx(var t: TTextRec): Word;
{$IFEND}
function _ReadChar(var t: TTextRec): AnsiChar;
function _ReadLong(var t: TTextRec): Longint;
procedure _ReadString(var t: TTextRec; s: PShortString; maxLen: Longint);
procedure _ReadCString(var t: TTextRec; s: PAnsiChar; maxLen: Longint);
procedure _ReadLString(var t: TTextRec; var s: AnsiString; CodePage: Word);
procedure _ReadUString(var t: TTextRec; var s: UnicodeString);
procedure _ReadWString(var t: TTextRec; var s: WideString);
procedure _ReadWCString(var t: TTextRec; s: PWideChar; maxBytes: Longint);
function _ReadWChar(var t: TTextRec): WideChar;
function _ReadExt(var t: TTextRec): Extended;
procedure _ReadLn(var t: TTextRec);
procedure _Rename(var f: TFileRec; newName: PChar);
procedure Release; deprecated;
function _ResetText(var t: TTextRec): Integer;
function _ResetFile(var f: TFileRec; recSize: Longint): Integer;
function _RewritText(var t: TTextRec): Integer;
function _RewritFile(var f: TFileRec; recSize: Longint): Integer;
procedure _RunError(errorCode: Byte);
procedure _Run0Error;
procedure _Seek(var f: TFileRec; recNum: Cardinal);
function _SeekEof(var t: TTextRec): Boolean;
function _SeekEoln(var t: TTextRec): Boolean;
procedure _SetTextBuf(var t: TTextRec; p: Pointer; size: Longint);
{$IFDEF PUREPASCAL}
function _StrLong(val, width: Longint): ShortString;
function _Str0Long(val: Longint): ShortString;
{$ELSE}
procedure _StrLong(val, width: Longint; s: PShortString);
procedure _Str0Long(val: Longint; s: PShortString);
{$ENDIF}
procedure _Truncate(var f: TFileRec);
function _ValLong(const s: string; var code: Integer): Longint;
{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure _UnhandledException;
{$ENDIF PC_MAPPED_EXCEPTIONS}
function _WriteRec(var f: TFileRec; buffer: Pointer): Pointer;
function _WriteChar(var t: TTextRec; c: AnsiChar; width: Integer): Pointer;
function _Write0Char(var t: TTextRec; c: AnsiChar): Pointer;
function _WriteBool(var t: TTextRec; val: Boolean; width: Longint): Pointer;
function _Write0Bool(var t: TTextRec; val: Boolean): Pointer;
function _WriteLong(var t: TTextRec; val, width: Longint): Pointer;
function _Write0Long(var t: TTextRec; val: Longint): Pointer;
function _WriteString(var t: TTextRec; const s: ShortString; width: Longint): Pointer;
function _Write0String(var t: TTextRec; const s: ShortString): Pointer;
function _WriteCString(var t: TTextRec; s: PAnsiChar; width: Longint): Pointer;
function _Write0CString(var t: TTextRec; s: PAnsiChar): Pointer;
function _Write0LString(var t: TTextRec; const s: AnsiString): Pointer;
function _WriteLString(var t: TTextRec; const s: AnsiString; width: Longint): Pointer;
function _Write0WString(var t: TTextRec; const s: WideString): Pointer;
function _WriteWString(var t: TTextRec; const s: WideString; width: Longint): Pointer;
function _WriteWCString(var t: TTextRec; s: PWideChar; width: Longint): Pointer;
function _Write0WCString(var t: TTextRec; s: PWideChar): Pointer;
function _WriteWChar(var t: TTextRec; c: WideChar; width: Integer): Pointer;
function _Write0WChar(var t: TTextRec; c: WideChar): Pointer;
function _WriteVariant(var T: TTextRec; const V: TVarData; Width: Integer): Pointer;
function _Write0Variant(var T: TTextRec; const V: TVarData): Pointer;
{$IF defined(CPU386)}
procedure _Write2Ext;
procedure _Write1Ext;
procedure _Write0Ext;
{$ELSE}
function _Write2Ext(var t: Text; val: Extended; width, prec: Longint): Pointer;
function _Write1Ext(var t: Text; val: Extended; width: Longint): Pointer;
function _Write0Ext(var t: Text; val: Extended): Pointer;
{$IFEND}
function _WriteLn(var t: TTextRec): Pointer;

procedure __CToPasStr(Dest: PShortString; const Source: PAnsiChar);
procedure __CLenToPasStr(Dest: PShortString; const Source: PAnsiChar; MaxLen: Integer);
procedure __ArrayToPasStr(Dest: PShortString; const Source: PAnsiChar; Len: Integer);
procedure __PasToCStr(const Source: PShortString; const Dest: PAnsiChar);


{ Compiler helper for set type support }
{$IF defined(CPU386)}
procedure _SetElem;
procedure _SetRange;
procedure _SetEq;
procedure _SetLe;
procedure _SetIntersect;
procedure _SetIntersect3; { BEG only }
procedure _SetUnion;
procedure _SetUnion3; { BEG only }
procedure _SetSub;
procedure _SetSub3; { BEG only }
procedure _SetExpand;
{$ELSE}
procedure _SetElem(var Dest {:Set}; Elem, Size: Integer);
procedure _SetRange(Lo, Hi, Size: Integer; var Dest {:Set});
function _SetEq(L, R: Pointer{PSet}; Size: Integer): Boolean;
function _SetLe(L, R: Pointer{PSet}; Size: Integer): Boolean;
procedure _SetIntersect(var Dest {:Set}; Src: Pointer{PSet}; Size: Integer);
procedure _SetIntersect3(var Dest {:Set}; L, R: Pointer{PSet}; Size: Integer);
procedure _SetUnion(var Dest {:Set}; Src: Pointer{PSet}; Size: Integer);
procedure _SetUnion3(var Dest {:Set}; L, R: Pointer{PSet}; Size: Integer);
procedure _SetSub(var Dest {:set}; const Src {:Set}; Size: Integer);
procedure _SetSub3(var Dest {:set}; L, R: Pointer{PSet}; Size: Integer);
procedure _SetExpand(Src: Pointer{PSet}; var Dest {:Set}; Lo, Hi: Integer);
{$IFEND}

{ Helper routines for standard procedure }
{$IF defined(CPU386)}
procedure _Str2Ext;
procedure _Str0Ext;
procedure _Str1Ext;
procedure _ValExt;
procedure _Pow10;
procedure _Real2Ext;
procedure _Ext2Real;
{$ELSE}
function _Str2Ext(val: Extended; width, precision: LongInt): ShortString;
function _Str0Ext(val: Extended): ShortString;
function _Str1Ext(val: Extended; width: LongInt): ShortString;
function _ValExt(s: string; var code: Integer): Extended;
function _Pow10(val: Extended; Power: Integer): Extended;
function _Real2Ext(val: Pointer {PReal48}): Extended;
function _Ext2Real(val: Extended): Real48;
{$IFEND}

{ Compiler helpers for object type support }
procedure _ObjSetup;
procedure _ObjCopy;
function _Fail(self: Pointer; allocFlag: LongInt): Pointer;
procedure _BoundErr;
procedure _IntOver;

{ Module initialization context.  For internal use only. }

type
  PInitContext = ^TInitContext;
  TInitContext = record
    OuterContext:   PInitContext;     { saved InitContext   }
{$IFNDEF PC_MAPPED_EXCEPTIONS}
    ExcFrame:       Pointer;          { bottom exc handler  }
{$ENDIF}
    InitTable:      PackageInfo;      { unit init info      }
    InitCount:      Integer;          { how far we got      }
    Module:         PLibModule;       { ptr to module desc  }
    DLLSaveEBP:     Pointer;          { saved regs for DLLs }
    DLLSaveEBX:     Pointer;          { saved regs for DLLs }
    DLLSaveESI:     Pointer;          { saved regs for DLLs }
    DLLSaveEDI:     Pointer;          { saved regs for DLLs }
{$IFDEF MSWINDOWS}
    ExitProcessTLS: procedure;        { Shutdown for TLS    }
{$ENDIF}
    DLLInitState:   Byte;             { 0 = package, 1 = DLL shutdown, 2 = DLL startup }
    ThreadID:       TThreadID;        { Initializing Thread }
  end platform;

type
  TDLLProc = procedure (Reason: Integer);
  // TDLLProcEx provides the reserved param returned by WinNT
  TDLLProcEx = procedure (Reason: Integer; Reserved: Pointer);

{$IF defined(LINUX) or defined(MACOS)}
procedure _StartExe(InitTable: PackageInfo; Module: PLibModule; Argc: Integer; Argv: Pointer);
procedure _StartLib(Context: PInitContext; Module: PLibModule; DLLProc: TDLLProcEx);
{$IFEND LINUX or MACOS}
{$IFDEF MSWINDOWS}
procedure _StartExe(InitTable: PackageInfo; Module: PLibModule);
{$IF defined(CPU386)}
procedure _StartLib;
{$ELSE}
procedure _StartLib(InitTable: PackageInfo; Module: PLibModule; TlsProc: Pointer; DllProc: TDllProcEx; AHInst: HINST; Reason: LongWord; Reserved: Pointer);
{$IFEND}
{$ENDIF}
procedure _PackageLoad(const Table : PackageInfo; Module: PLibModule);
procedure _PackageUnload(const Table : PackageInfo; Module: PLibModule);

{$IF not defined(CPU386)}
type
  _PResStringInitTableElem = ^_TResStringInitTableElem;
  _TResStringInitTableElem = record
    const // stringKind
      LString = 0;
      WString = 1;
      UString = 2;
    var
      variableAddress: Pointer;
      resStringAddress: PResStringRec;
      stringKind: Integer;
  end;
  _PResStringInitTable = ^_TResStringInitTable;
  _TResStringInitTable = record
    Count: Integer;
    Table: array[1..(MaxInt div SizeOf(_TResStringInitTableElem) - 2)] of _TresStringInitTableElem;
  end;
  {$NODEFINE _PResStringInitTableElem}
  {$NODEFINE _TResStringInitTableElem}
  {$NODEFINE _PResStringInitTable}
  {$NODEFINE _TResStringInitTable}

  _PResStringImportInitTableElem = ^_TResStringImportInitTableElem;
  _TResStringImportInitTableElem = record
    const // stringKind
      LString = 0;
      WString = 1;
      UString = 2;
    var
      variableAddress: Pointer;
      resStringIndirAddress: ^PResStringRec;
      stringKind: Integer;
  end;
  _PResStringImportInitTable = ^_TResStringImportInitTable;
  _TResStringImportInitTable = record
    Count: Integer;
    Table: array[1..(MaxInt div SizeOf(_TResStringImportInitTableElem) - 2)] of _TResStringImportInitTableElem;
  end;
  {$NODEFINE _PResStringImportInitTableElem}
  {$NODEFINE _TResStringImportInitTableElem}
  {$NODEFINE _PResStringImportInitTable}
  {$NODEFINE _TResStringImportInitTable}

  _PImportInitTableElem = ^_TImportInitTableElem;
  _TImportInitTableElem = record
    variableAddress: Pointer;
    sourceIndirAddress: PPointer;
    soruceOffset: NativeInt;
  end;
  _PImportInitTable = ^_TImportInitTable;
  _TImportInitTable = record
    Count: Integer;
    Table: array[1..(MaxInt div SizeOf(_TImportInitTableElem) - 2)] of _TImportInitTableElem;
  end;
  {$NODEFINE _PImportInitTableElem}
  {$NODEFINE _TImportInitTableElem}
  {$NODEFINE _PImportInitTable}
  {$NODEFINE _TImportInitTable}

  _PWideStringInitTableElem = ^_TWideStringInitTableElem;
  _TWideStringInitTableElem = record
    variableAddress: Pointer;
    stringAddress: Pointer;
  end;
  _PWideStringInitTable = ^_TWideStringInitTable;
  _TWideStringInitTable = record
    Count: Integer;
    Table: array[1..(MaxInt div SizeOf(_TWideStringInitTableElem) - 2)] of _TWideStringInitTableElem;
  end;
  {$NODEFINE _PWideStringInitTableElem}
  {$NODEFINE _TWideStringInitTableElem}
  {$NODEFINE _PWideStringInitTable}
  {$NODEFINE _TWideStringInitTable}
{$IFEND}

{$IFDEF MSWINDOWS}
type
  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord = record
    ExceptionCode: Cardinal;
    ExceptionFlags: Cardinal;
    ExceptionRecord: PExceptionRecord;
    ExceptionAddress: Pointer;
    NumberParameters: Cardinal;
    case {IsOsException:} Boolean of
      True:  (ExceptionInformation : array [0..14] of NativeUInt);
      False: (ExceptAddr: Pointer; ExceptObject: Pointer);
  end;
  TExceptClsProc = function(P: PExceptionRecord): Pointer{ExceptClass};
  TExceptObjProc = function(P: PExceptionRecord): Pointer{Exception};
  TRaiseExceptObjProc = procedure(P: PExceptionRecord);

{$IF not defined(CPU386)}
  PContext = Pointer{^TContext};
  PExceptionPointers = ^TExceptionPointers;
  TExceptionPointers = record
    ExceptionRecord: PExceptionRecord;
    ContextRecord: PContext;
  end;
  _TDelphiFinallyHandlerProc = function(ExceptionPointers: PExceptionPointers;
                                        EstablisherFrame: NativeUInt): Integer;
  _TExceptionHandlerProc = function(ExceptionPointers: PExceptionPointers;
                                    EstablisherFrame: NativeUInt): Integer;
  _TDelphiCatchHandlerProc = function(ExceptionPointers: PExceptionPointers;
                                 EstablisherFrame: NativeUInt; ExceptionObject: Pointer): NativeUInt;
{$IFEND}
{$ENDIF}
{$IFDEF PC_MAPPED_EXCEPTIONS}
type
  PRaisedException = ^TRaisedException;
  TRaisedException = packed record
    RefCount: Integer;
    ExceptObject: TObject;
    ExceptionAddr: Pointer;
    HandlerEBP: LongWord;
    Flags: LongWord;
    Cleanup: Pointer;
    Prev: PRaisedException;
    ReleaseProc: Pointer;
  end;
  PExceptionRecord = PRaisedException;
  TExceptionRecord = TRaisedException;
{$ENDIF}

{$IF defined(CPU386)}
procedure _InitResStrings;
procedure _InitResStringImports;
procedure _InitImports;
{$IFDEF MSWINDOWS}
procedure _InitWideStrings;
{$ENDIF}
{$ELSE}
procedure _InitResStrings(InitTable: _PResStringInitTable);
procedure _InitResStringImports(InitTable: _PResStringImportInitTable);
procedure _InitImports(InitTable: _PImportInitTable);
{$IFDEF MSWINDOWS}
procedure _InitWideStrings(InitTable: _PWideStringInitTable);
{$ENDIF}
{$IFEND}

{$IF not defined(CPU386)}
function _ClassCreate(InstanceOrVMT: Pointer; Alloc: ShortInt): TObject;
{$ELSE}
function _ClassCreate(AClass: TClass; Alloc: Boolean): TObject;
{$IFEND}
procedure _ClassDestroy(Instance: TObject);
function _AfterConstruction(Instance: TObject): TObject;
{$IF defined(CPU386)}
function _BeforeDestruction(Instance: TObject; OuterMost: ShortInt): TObject;
{$ELSE}
procedure _BeforeDestruction(Instance: TObject; OuterMost: ShortInt);
{$IFEND}
function _IsClass(Child: TObject; Parent: TClass): Boolean;
function _AsClass(Child: TObject; Parent: TClass): TObject;
function _GetHelperIntf(Instance: TObject; HelperClass: TClass): IInterface;
function _IntfAsClass(const Intf: IInterface; Parent: TClass): TObject;
function _SafeIntfAsClass(const Intf: IInterface; Parent: TClass): TObject;
function _IntfIsClass(const Intf: IInterface; Parent: TClass): Boolean;

{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure _RaiseAtExcept;
//procedure _DestroyException(Exc: PRaisedException);
procedure _DestroyException;
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IF defined(CPU386)}
procedure _RaiseExcept;
{$ELSE !CPU386}
procedure _RaiseExcept(Obj: TObject);
procedure _RaiseAtExcept(Obj: TObject; Address: NativeUInt);
{$IFEND !CPU386}
procedure _RaiseAgain;
procedure _DoneExcept;
{$IFNDEF PC_MAPPED_EXCEPTIONS}
procedure _TryFinallyExit;
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IF defined(CPU386)}
procedure _HandleAnyException;
procedure _HandleOnException;
procedure _HandleFinally;
{$IFEND CPU386}
{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure _HandleOnExceptionPIC;
{$ENDIF PC_MAPPED_EXCEPTIONS}
procedure _HandleAutoException;
{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure _ClassHandleException;
{$ENDIF PC_MAPPED_EXCEPTIONS}

{$IF defined(MSWINDOWS) and (not defined(CPU386))}
function _X64DelphiExceptionHandler(ExceptionRecord: PExceptionRecord;
  EstablisherFrame: NativeUInt; ContextRecord: PContext;
  DispatcherContext: Pointer{PDispatcherContext}):
  LongInt{TExceptionDisposition}; stdcall;
{$IFEND MSWINDOWS and (not CPU386)}

{$IF defined(CPU386)}
procedure _CallDynaInst;
procedure _CallDynaClass;
procedure _FindDynaInst;
procedure _FindDynaClass;
{$ELSE}


function _FindDynaInst(Self: TObject; Selector: SmallInt): Pointer;
function _FindDynaClass(Vmt: TClass; Selector: SmallInt): Pointer;
{$IFEND}

{ STRING SUPPORT }

{ Compiler helper for string allocation and release }
function _NewUnicodeString(CharLength: LongInt): Pointer;
function _NewAnsiString(CharLength: LongInt; CodePage: Word): Pointer;
function _NewWideString(CharLength: LongInt): Pointer;
{$IFDEF PUREPASCAL}
function _UStrClr(var S): Pointer;
function _LStrClr(var S): Pointer;
function _WStrClr(var S): Pointer;
{$ELSE}
procedure _UStrClr(var S);
procedure _LStrClr(var S);
procedure _WStrClr(var S);
{$ENDIF}
procedure _UStrArrayClr(var StrArray; Count: Integer);
procedure _LStrArrayClr(var StrArray; cnt: longint);
procedure _WStrArrayClr(var StrArray; Count: Integer);
function _UStrAddRef(Str: Pointer): Pointer;
function _LStrAddRef(Str: Pointer): Pointer;
{$IFDEF MSWINDOWS}
function _WStrAddRef(var Str: WideString): Pointer;
{$ELSE}
function _WStrAddRef(Str: Pointer): Pointer;
{$ENDIF}

{ Compiler helper for basic string constructors }
procedure _UStrFromPWCharLen(var Dest: UnicodeString; Source: PWideChar; CharLength: Integer);
procedure _WStrFromPWCharLen(var Dest: WideString; Source: PWideChar; CharLength: Integer);
procedure _LStrFromPCharLen(var Dest: AnsiString; Source: PAnsiChar; Length: Integer; CodePage: Word);
//procedure InternalUStrFromPCharLen(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer; CodePage: Integer);
procedure _UStrFromPCharLen(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer);
//procedure InternalWStrFromPCharLen(var Dest: WideString; Source: PAnsiChar; Length: Integer; CodePage: Integer);
procedure _WStrFromPCharLen(var Dest: WideString; Source: PAnsiChar; Length: Integer);
procedure _LStrFromPWCharLen(var Dest: AnsiString; Source: PWideChar; Length: Integer; CodePage: Word);

{ Compiler helper for string assignment }
procedure _UStrAsg(var Dest: UnicodeString; const Source: UnicodeString);
procedure _UStrLAsg(var Dest: UnicodeString; const Source: UnicodeString);
procedure _WStrAsg(var Dest: WideString; const Source: WideString);
procedure _WStrLAsg(var Dest: WideString; const Source: WideString);
{$IFDEF PUREPASCAL}
procedure _LStrAsg(var Dest: AnsiString; const Source: AnsiString);
procedure _LStrLAsg(var Dest: AnsiString; const Source: AnsiString);
{$ELSE}
// Note: VCLE.LIB|dstring.cpp refers mangled names
procedure _LStrAsg(var dest; const source);
procedure _LStrLAsg(var dest; const source);
{$ENDIF}

{ string info utilities }
function StringElementSize(const S: UnicodeString): Word; overload; inline;
function StringElementSize(const S: RawByteString): Word; overload; inline;
function StringCodePage(const S: UnicodeString): Word; overload; inline;
function StringCodePage(const S: RawByteString): Word; overload; inline;
function StringRefCount(const S: UnicodeString): Integer; overload; inline;
function StringRefCount(const S: RawByteString): Integer; overload; inline;
{$IFNDEF MSWINDOWS}
function StringElementSize(const S: WideString): Word; overload; inline;
function StringCodePage(const S: WideString): Word; overload; inline;
function StringRefCount(const S: WideString): Integer; overload; inline;
{$ENDIF}

{ Compiler helper for string length }
function _UStrLen(const S: UnicodeString): Integer; inline;
function _WStrLen(const S: WideString): Longint; inline;
// Note: VCLE.LIB|dstring.cpp refers mangled names of _LStrLen
function _LStrLen(const S: AnsiString): Longint; inline;
//function _PStrLen(const S: ShortString): Integer; inline;
function _PCharLen(P: PAnsiChar): Integer;
function _PWCharLen(P: PWideChar): Integer;

{ Compiler helper for _UniqueString* functions }
function _UniqueStringU(var Str: UnicodeString): Pointer;
{$IFNDEF MSWINDOWS}
function _UniqueStringW(var Str: WideString): Pointer;
{$ENDIF}
function _UniqueStringA(var Str: AnsiString): Pointer;

{ UniqueString* functions }
procedure UniqueString(var Str: UnicodeString); overload;
procedure UniqueString(var Str: WideString); overload;
procedure UniqueString(var Str: AnsiString); overload;

{ Compiler helper for comparing array of characters }
{$IF defined(CPU386)}
procedure _PStrCmp;
procedure _AStrCmp;
procedure _WStrLCmp;
{$ELSE}
function _PStrCmp(const Left, Right: ShortString): Integer;
function _AStrCmp(const Left, Right: PAnsiChar; Len: NativeInt): Integer;
function _WStrLCmp(const Left, Right: PWideChar; Len: NativeInt): Integer;
{$IFEND}

{ Compiler helper for ShortString support }
procedure _PStrCpy(Dest: PShortString; Source: PShortString);
procedure _PStrNCpy(Dest: PShortString; Source: PShortString; MaxLen: Byte);
{$IF defined(CPU386)}
procedure _PStrCat;
procedure _PStrNCat;
{$ELSE}
procedure _PStrCat(Dest: PShortString; const Src: ShortString);
procedure _PStrNCat(Dest: PShortString; const Src: ShortString; Size:Integer);
{$IFEND}
{$IF defined(CPU386)}
procedure _Copy{ s : ShortString; index, count : Integer ) : ShortString};
procedure _Delete{ var s : openstring; index, count : Integer };
procedure _Insert{ source : ShortString; var s : openstring; index : Integer };
//procedure _Pos{ substr : ShortString; s : ShortString ) : Integer};
{$ELSE}
function _Copy(const S: ShortString; Index, Count: Integer): ShortString;
procedure _Delete(var S: OpenString; Index, Count: Integer);
procedure _Insert(const Source: ShortString; var S: OpenString; Index: Integer);
{$IFEND}
procedure _SetLength(s: PShortString; newLength: Byte);
procedure _SetString(s: PShortString; buffer: PAnsiChar; len: Byte);

{ Compiler helper for AnsiString support }
// Note: VCLE.LIB|dstring.cpp refers mangled names of _LStrFromArray, _LStrFromWStr, _LStrFromUStr, _LStrFromPChar and _LStrFromPWChar
procedure _LStrFromChar(var Dest: AnsiString; Source: AnsiChar; CodePage: Word);
procedure _LStrFromWChar(var Dest: AnsiString; Source: WideChar; CodePage: Word);
procedure _LStrFromPChar(var Dest: AnsiString; Source: PAnsiChar; CodePage: Word);
procedure _LStrFromPWChar(var Dest: AnsiString; Source: PWideChar; CodePage: Word);
procedure _LStrFromString(var Dest: AnsiString; const Source: ShortString; CodePage: Word);
procedure _LStrFromArray(var Dest: AnsiString; Source: PAnsiChar; Length: Integer; CodePage: Word);
procedure _LStrFromWArray(var Dest: AnsiString; Source: PWideChar; Length: Integer; CodePage: Word);
procedure _LStrFromWStr(var Dest: AnsiString; const Source: WideString; CodePage: Word);
procedure _LStrFromUStr(var Dest: AnsiString; const Source: UnicodeString; CodePage: Word);
{$IF defined(CPU386)}
procedure _LStrToString{(var Dest: ShortString; const Source: AnsiString; MaxLen: Integer)};
{$ELSE}
procedure _LStrToString(Dest: PShortString; const Source: AnsiString; MaxLen: Integer);
{$IFEND}
{$IF defined(CPU386)}
procedure _LStrCat{var dest: AnsiString; source: AnsiString};
procedure _LStrCat3{var dest:AnsiString; source1: AnsiString; source2: AnsiString};
procedure _LStrCatN{var dest:AnsiString; argCnt: Integer; ...};
procedure _LStrCmp{left: AnsiString; right: AnsiString};
procedure _LStrEqual{const Left, Right: AnsiString};
{$ELSE}
procedure _LStrCat(var Dest: AnsiString; const Source: AnsiString);
procedure _LStrCat3(var Dest:AnsiString; const Source1, Source2: AnsiString);
procedure _LStrCatN(var Dest: AnsiString; ArgCnt: Integer; const Strs: AnsiString); varargs;
function _LStrCmp(const Left, Right: AnsiString): Integer;
function _LStrEqual(const Left, Right: AnsiString): Integer;
{$IFEND}
function _LStrToPChar(const S: AnsiString): PAnsiChar;
{$IF defined(CPU386)}
procedure _LStrCopy  { const s : AnsiString; index, count : Integer) : AnsiString};
procedure _LStrDelete{ var s : AnsiString; index, count : Integer };
procedure _LStrInsert{ const source : AnsiString; var s : AnsiString; index : Integer };
procedure _LStrPos{ const substr : AnsiString; const s : AnsiString ) : Integer};
procedure _LStrSetLength{ var str: AnsiString; newLength: Integer; CodePage: Word };
{$ELSE}
function _LStrCopy(const Str: AnsiString; Index, Count: Integer): AnsiString;
procedure _LStrDelete(var S: AnsiString; Index, Count: Integer);
procedure _LStrInsert(const Source: AnsiString; var S: AnsiString; Index: Integer);
function _LStrPos(const SubStr: AnsiString; const S: AnsiString): Integer;
procedure _LStrSetLength(var Str: AnsiString; NewLength: Integer; CodePage: Word);
{$IFEND}

{ Compiler helper for WideString support }
function _WStrToPWChar(const S: WideString): PWideChar;
procedure _WStrFromChar(var Dest: WideString; Source: AnsiChar);
procedure _WStrFromWChar(var Dest: WideString; Source: WideChar);
procedure _WStrFromPChar(var Dest: WideString; Source: PAnsiChar);
procedure _WStrFromPWChar(var Dest: WideString; Source: PWideChar);
procedure _WStrFromString(var Dest: WideString; const Source: ShortString);
procedure _WStrFromArray(var Dest: WideString; Source: PAnsiChar; Length: Integer);
procedure _WStrFromWArray(var Dest: WideString; Source: PWideChar; Length: Integer);
procedure _WStrFromLStr(var Dest: WideString; const Source: AnsiString);
procedure _WStrFromUStr(var Dest: WideString; const Source: UnicodeString);
procedure _WStrToString(Dest: PShortString; const Source: WideString; MaxLen: Integer);
procedure _WStrCat(var Dest: WideString; const Source: WideString);
procedure _WStrCat3(var Dest: WideString; const Source1, Source2: WideString);
{$IF defined(CPU386)}
procedure _WStrCatN{var dest:WideString; argCnt: Integer; ...};
procedure _WStrCmp{left: WideString; right: WideString};
procedure _WStrEqual{const Left, Right: WideString};
{$ELSE}
procedure _WStrCatN{var dest:WideString; argCnt: Integer; ...};
function _WStrCmp(const Left, Right: WideString): Integer;
function _WStrEqual(const Left, Right: WideString): Integer;
{$IFEND}
function _WStrCopy(const S: WideString; Index, Count: Integer): WideString;
procedure _WStrDelete(var S: WideString; Index, Count: Integer);
procedure _WStrInsert(const Source: WideString; var Dest: WideString; Index: Integer);
procedure _WStrSetLength(var S: WideString; NewLength: Integer);
procedure _WCharToString(Dest: PShortString; const Source: WideChar; MaxLen: Integer);

{ Compiler helper for UnicodeString support }
function _UStrToPWChar(const S: UnicodeString): PWideChar;
procedure _UStrFromChar(var Dest: UnicodeString; Source: AnsiChar);
procedure _UStrFromWChar(var Dest: UnicodeString; Source: WideChar);
procedure _UStrFromPChar(var Dest: UnicodeString; Source: PAnsiChar);
procedure _UStrFromPWChar(var Dest: UnicodeString; Source: PWideChar);
procedure _UStrFromArray(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer);
procedure _UStrFromWArray(var Dest: UnicodeString; Source: PWideChar; Length: Integer);
procedure _UStrFromLStr(var Dest: UnicodeString; const Source: AnsiString);
procedure _UStrFromWStr(var Dest: UnicodeString; const Source: WideString);
procedure _UStrToString(Dest: PShortString; const Source: UnicodeString; MaxLen: Integer);
procedure _UStrFromString(var Dest: UnicodeString; const Source: ShortString);
procedure _UStrSetLength(var Str: UnicodeString; NewLength: Integer);
procedure _UStrCat(var Dest: UnicodeString; const Source: UnicodeString);
procedure _UStrCat3(var Dest: UnicodeString; const Source1, Source2: UnicodeString);
procedure _UStrCatN{var dest:UnicodeString; argCnt: Integer; ...};
{$IF defined(CPU386)}
procedure _UStrCmp{const Left, Right: UnicodeString};
procedure _UStrEqual{const Left, Right: UnicodeString};
{$ELSE}
function _UStrCmp(const Left, Right: UnicodeString): Integer;
function _UStrEqual(const Left, Right: UnicodeString): Integer;
{$IFEND}
function _UStrCopy(const S: UnicodeString; Index, Count: Integer): UnicodeString;
procedure _UStrDelete(var S: UnicodeString; Index, Count: Integer);
procedure _UStrInsert(const Source: UnicodeString; var Dest: UnicodeString; Index: Integer);

{ string utilities }
function Pos(const SubStr, Str: ShortString): Integer; overload;
function Pos(const SubStr, Str: UnicodeString): Integer; overload;
function Pos(const SubStr, Str: WideString): Integer; overload;
function Pos(const SubStr, Str: RawByteString): Integer; overload;
function StringOfChar(Ch: WideChar; Count: Integer): UnicodeString; overload;
function StringOfChar(Ch: AnsiChar; Count: Integer): AnsiString; overload;
procedure SetAnsiString(Dest: PAnsiString; Source: PWideChar; Length: Integer; CodePage: Word);
procedure SetCodePage(var S: RawByteString; CodePage: Word; Convert: Boolean = True);
function UnicodeStringToUCS4String(const S: UnicodeString): UCS4String;
function UCS4StringToUnicodeString(const S: UCS4String): UnicodeString;
function WideCharToUCS4String(S: PWideChar; Len: Integer = MaxInt): UCS4String;
//function UTF8Encode(const WS: UnicodeString): UTF8String;
//function UTF8Decode(const S: UTF8String): UnicodeString;

function _Write0UString(var t: TTextRec; const s: UnicodeString): Pointer;
function _WriteUString(var t: TTextRec; const s: UnicodeString; width: Longint): Pointer;


{ Compiler helper for initializing/finalizing variable }
procedure InitializeArray(p: Pointer; typeInfo: Pointer; elemCount: Cardinal);
procedure _Initialize(p: Pointer; typeInfo: Pointer);
procedure _InitializeArray(p: Pointer; typeInfo: Pointer; elemCount: Cardinal);
procedure _InitializeRecord(p: Pointer; typeInfo: Pointer);
{$IFDEF PUREPASCAL}
function _Finalize(P: Pointer; TypeInfo: Pointer): Pointer;
procedure FinalizeArray(P: Pointer; TypeInfo: Pointer; Count: Cardinal);
function _FinalizeArray(P: Pointer; TypeInfo: Pointer; ElemCount: Cardinal): Pointer;
function _FinalizeRecord(P: Pointer; TypeInfo: Pointer): Pointer;
{$ELSE PUREPASCAL}
procedure _Finalize(p: Pointer; typeInfo: Pointer);
procedure FinalizeArray(P: Pointer; TypeInfo: Pointer; Count: Cardinal);
procedure _FinalizeArray(P: Pointer; TypeInfo: Pointer; ElemCount: Cardinal);
procedure _FinalizeRecord(P: Pointer; TypeInfo: Pointer);
{$ENDIF !PUREPASCAL}
{$IF defined(CPU386)}
procedure _AddRef;
procedure _AddRefArray;
procedure _AddRefRecord;
{$ELSE}
procedure _AddRef(P: Pointer; TypeInfo: Pointer);
procedure _AddRefArray(P: Pointer; TypeInfo: Pointer; ElemCount: Longint);
procedure _AddRefRecord(P: Pointer; TypeInfo: Pointer);
{$IFEND}
procedure CopyArray(dest, source, typeInfo: Pointer; cnt: Integer);
{$IF defined(CPU386)}
procedure _CopyArray;
procedure _CopyRecord;
procedure _CopyObject;
{$ELSE}
procedure _CopyArray(Dest, Source, TypeInfo: Pointer; Count: Integer);
procedure _CopyRecord(Dest, Source, TypeInfo: Pointer);
procedure _CopyObject(Dest, Source: Pointer; vmtPtrOffs: LongInt; TypeInfo: Pointer);
{$IFEND}

function _New(size: Longint; typeInfo: Pointer): Pointer;
procedure _Dispose(p: Pointer; typeInfo: Pointer);

{ 64-bit Integer helper routines }
{$IF defined(CPU386)}
procedure __llmul;
procedure __lldiv;
procedure __lludiv;
procedure __llmod;
procedure __llmulo;
procedure __lldivo;
procedure __llmodo;
procedure __llumod;
procedure __llshl;
procedure __llushr;
{$IFEND}
{$IF defined(CPU386)}
procedure _WriteInt64;
procedure _Write0Int64;
procedure _WriteUInt64;
procedure _Write0UInt64;
procedure _ReadInt64;
{$ELSE}
function _WriteInt64(var t: TTextRec; val: Int64; width: LongInt): Pointer;
function _Write0Int64(var t: TTextRec; val: Int64): Pointer;
function _WriteUInt64(var t: TTextRec; val: UInt64; width: LongInt): Pointer;
function _Write0UInt64(var t: TTextRec; val: UInt64): Pointer;
function _ReadInt64(var t: TTextRec): Int64;
{$IFEND}
function _StrInt64(val: Int64; width: Integer): ShortString;
function _Str0Int64(val: Int64): ShortString;
function _ValInt64(const s: string; var code: Integer): Int64;
function _StrUInt64(val: UInt64; width: Integer): ShortString;
function _Str0UInt64(val: Int64): ShortString;

{ Compiler helper for Dynamic array support }
{$IF defined(CPU386)}
procedure _DynArrayHigh;
procedure _DynArrayClear(var a: Pointer; typeInfo: Pointer);
procedure _DynArrayLength;
procedure _DynArraySetLength;

procedure _DynArrayCopy(a: Pointer; typeInfo: Pointer; var Result: Pointer);
procedure _DynArrayCopyRange(A: Pointer; TypeInfo: Pointer; Index, Count : Integer; var Result: Pointer);

procedure _DynArrayAsg;
procedure _DynArrayAddRef;
{$ELSE}
function _DynArrayHigh(const A: Pointer): LongInt;
function _DynArrayClear(var A: Pointer; TypeInfo: Pointer): Pointer;
function _DynArrayLength(const A: Pointer): LongInt;
procedure _DynArraySetLength(var A: Pointer; TypeInfo: Pointer; DimCnt: LongInt; LengthVec: LongInt);

procedure _DynArrayCopy(var Result: Pointer; a: Pointer; typeInfo: Pointer);
procedure _DynArrayCopyRange(var Result: Pointer; A: Pointer; TypeInfo: Pointer; Index, Count : Integer);

procedure _DynArrayAsg(var Dest: Pointer; Src: Pointer; TypeInfo: Pointer);
procedure _DynArrayAddRef(P: Pointer);
{$IFEND}

procedure DynArrayClear(var a: Pointer; typeInfo: Pointer);
procedure DynArraySetLength(var a: Pointer; typeInfo: Pointer; dimCnt: Longint; lengthVec: PLongint);
function DynArrayDim(typeInfo: PDynArrayTypeInfo): Integer;
function DynArraySize(A: Pointer): Integer;
{$NODEFINE DynArrayDim}

function _IntfClear(var Dest: IInterface): Pointer;
procedure _IntfCopy(var Dest: IInterface; const Source: IInterface);
procedure _IntfCast(var Dest: IInterface; const Source: IInterface; const IID: TGUID);
procedure _IntfAddRef(const Dest: IInterface);

{$IFDEF WIN32}
procedure _FSafeDivide;
procedure _FSafeDivideR;
{$ENDIF}

function _CheckAutoResult(ResultCode: HResult): HResult;

{$IF defined(CPU386)}
procedure FPower10;
{$ELSE}
function FPower10(val: Extended; power: Integer): Extended;
{$IFEND}

procedure TextStart; deprecated;

// Conversion utility routines for C++ convenience.  Not for Delphi code.
function  CompToDouble(Value: Comp): Double; cdecl;
procedure DoubleToComp(Value: Double; var Result: Comp); cdecl;
function  CompToCurrency(Value: Comp): Currency; cdecl;
procedure CurrencyToComp(Value: Currency; var Result: Comp); cdecl;

function GetMemory(Size: Integer): Pointer; cdecl;
function FreeMemory(P: Pointer): Integer; cdecl;
function ReallocMemory(P: Pointer; Size: Integer): Pointer; cdecl;


{ Internal runtime error codes }

type
  TRuntimeError = (reNone, reOutOfMemory, reInvalidPtr, reDivByZero,
  reRangeError, reIntOverflow, reInvalidOp, reZeroDivide, reOverflow,
  reUnderflow, reInvalidCast, reAccessViolation, rePrivInstruction,
  reControlBreak, reStackOverflow,
  { reVar* used in Variants.pas }
  reVarTypeCast, reVarInvalidOp,
  reVarDispatch, reVarArrayCreate, reVarNotArray, reVarArrayBounds,
  reAssertionFailed,
  reExternalException, { not used here; in SysUtils }
  reIntfCastError, reSafeCallError,
  reMonitorNotLocked, reNoMonitorSupport
{$IF defined(LINUX) or defined(MACOS)}
  , reQuit
{$IFEND LINUX or MACOS}
{$IFDEF POSIX}
  , reCodesetConversion
{$ENDIF POSIX}
  , rePlatformNotImplemented
  );
{$NODEFINE TRuntimeError}

procedure Error(errorCode: TRuntimeError);
{$NODEFINE Error}

{ GetLastError returns the last error reported by an OS API call.  Calling
  this function usually resets the OS error state.
}

function GetLastError: Integer; {$IFDEF MSWINDOWS} stdcall; {$ENDIF}
{$EXTERNALSYM GetLastError}

{ SetLastError writes to the thread local storage area read by GetLastError. }

procedure SetLastError(ErrorCode: Integer); {$IFDEF MSWINDOWS} stdcall; {$ENDIF}

{$IF defined(LINUX) or defined(MACOS)}
{  To improve performance, some RTL routines cache module handles and data
   derived from modules.  If an application dynamically loads and unloads
   shared object libraries, packages, or resource packages, it is possible for
   the handle of the newly loaded module to match the handle of a recently
   unloaded module.  The resource caches have no way to detect when this happens.

   To address this issue, the RTL maintains an internal counter that is
   incremented every time a module is loaded or unloaded using RTL functions
   (like LoadPackage).  This provides a cache version level signature that
   can detect when modules have been cycled but have the same handle.

   If you load or unload modules "by hand" using dlopen or dlclose, you must call
   InvalidateModuleCache after each load or unload so that the RTL module handle
   caches will refresh themselves properly the next time they are used.  This is
   especially important if you manually tinker with the LibModuleList list of
   loaded modules, or manually add or remove resource modules in the nodes
   of that list.

   ModuleCacheID returns the "current generation" or version number kept by
   the RTL.  You can use this to implement your own refresh-on-next-use
   (passive) module handle caches as the RTL does.  The value changes each
   time InvalidateModuleCache is called.
}

function ModuleCacheID: Cardinal;
procedure InvalidateModuleCache;
{$IFEND LINUX or MACOS}

procedure SetMultiByteConversionCodePage(CodePage: Integer);

function GetUILanguages(const LANGID: WORD): string;
function GetLocaleOverride(const AppName: string): string;
procedure SetLocaleOverride(const NewPreferredLanguages: string);

{$IFDEF MSWINDOWS}
type
  PImageThunkData = ^TImageThunkData;
  TImageThunkData = record
    case Byte of
      0: (ForwarderString: LongWord); // PBYTE
      1: (_Function: LongWord);       // PLongWord Function -> _Function
      2: (Ordinal: LongWord);
      3: (AddressOfData: LongWord);   // PIMAGE_IMPORT_BY_NAME
  end;
  {$EXTERNALSYM TImageThunkData}
  {$EXTERNALSYM PImageThunkData}

  ImgDelayDescr = record
    grAttrs:     LongWord;          { attributes                        }
    szName:      PAnsiChar;         { pointer to dll name               }
    hmod:        HMODULE;           { address of module handle          }
    pIAT:        PImageThunkData;   { address of the IAT                }
    pINT:        PImageThunkData;   { address of the INT                }
    pBoundIAT:   PImageThunkData;   { address of the optional bound IAT }
    pUnloadIAT:  PImageThunkData;   { address of optional copy of
                                       original IAT                     }
    dwTimeStamp: LongWord;          { 0 if not bound,                   }
                                    { O.W. date/time stamp of DLL bound
                                       to (Old BIND)                    }
  end;
  {$EXTERNALSYM ImgDelayDescr}
  TImgDelayDescr = ImgDelayDescr;
  PImgDelayDescr = ^TImgDelayDescr;

{ Delay load import hook notifications }

  dliNotification = (
    dliNoteStartProcessing,        { used to bypass or note helper only     }
    dliNotePreLoadLibrary,         { called just before LoadLibrary, can    }
                                   {  override w/ new HMODULE return val    }
    dliNotePreGetProcAddress,      { called just before GetProcAddress, can }
                                   {  override w/ new Proc address return   }
                                   {  value                                 }
    dliFailLoadLibrary,            { failed to load library, fix it by      }
                                   {  returning a valid HMODULE             }
    dliFailGetProcAddress,         { failed to get proc address, fix it by  }
                                   {  returning a valid Proc address        }
    dliNoteEndProcessing           { called after all processing is done,   }
                                   {  no bypass possible at this point      }
                                   {  except by raise, or
                                       RaiseException.                       }
  );
  {$EXTERNALSYM dliNotification}

  DelayLoadProc = record
    fImportByName:      LongBool;
    case Byte of
      0: (szProcName:   PAnsiChar);
      1: (dwOrdinal:    LongWord);
  end;
  {$EXTERNALSYM DelayLoadProc}
  TDelayLoadProc = DelayLoadProc;
  PDelayLoadProc = ^TDelayLoadProc;

  DelayLoadInfo = record
    cb:          LongWord;       { size of structure                 }
    pidd:        PImgDelayDescr; { raw form of data (everything is
                                   there)                            }
    ppfn:        Pointer;        { points to address of function to
                                   load                              }
    szDll:       PAnsiChar;      { name of dll                       }
    dlp:         TDelayLoadProc; { name or ordinal of procedure      }
    hmodCur:     HMODULE;        { the hInstance of the library we
                                   have loaded                       }
    pfnCur:      Pointer;        { the actual function that will be
                                   called                            }
    dwLastError: LongWord;       { error received (if an error
                                   notification)                     }
  end;
  {$EXTERNALSYM DelayLoadInfo}
  TDelayLoadInfo = DelayLoadInfo;
  PDelayLoadInfo = ^TDelayLoadInfo;

  DelayedLoadHook = function (dliNotify: dliNotification; pdli: PDelayLoadInfo): Pointer; stdcall;
  {$EXTERNALSYM DelayedLoadHook}
  TDelayedLoadHook = DelayedLoadHook;

procedure ___pfnDliNotifyHook;
procedure ___pfnDliFailureHook;
procedure __delayLoadHelper;
procedure __FUnloadDelayLoadedDLL;

{ Unload support }

var
  UnloadDelayLoadedDLLPtr: Pointer = @__FUnloadDelayLoadedDLL;
  DelayLoadHelper: Pointer = @__delayLoadHelper;
  pfnDliNotifyHook: Pointer = @___pfnDliNotifyHook;
  pfnDliFailureHook: Pointer = @___pfnDliFailureHook;

{ Hook pointers }

{ The "notify hook" gets called for every call to the
   delay load helper.  This allows a user to hook every call and
   skip the delay load helper entirely.

   dliNotify =
   (
       dliNoteStartProcessing   or
       dliNotePreLoadLibrary    or
       dliNotePreGetProcAddress or
       dliNoteEndProcessing
   )

   on this call.
}

function SetDliNotifyHook(HookProc: TDelayedLoadHook): TDelayedLoadHook;
function DliNotifyHook: TDelayedLoadHook;
{$EXTERNALSYM SetDliNotifyHook}
{$EXTERNALSYM DliNotifyHook}

{ This is the failure hook,

   dliNotify =
   (
       dliFailLoadLibrary       or
       dliFailGetProcAddress
   )
}
function SetDliFailureHook(HookProc: TDelayedLoadHook): TDelayedLoadHook;
function DliFailureHook: TDelayedLoadHook;
{$EXTERNALSYM SetDliFailureHook}
{$EXTERNALSYM DliFailureHook}

{ takes a pointer to a name to unload, or NULL to unload all the delay load dlls in the list. }

procedure UnloadDelayLoadedDLL(szDll: PAnsiChar); stdcall;

procedure _delayLoadHelper;
{$ENDIF MSWINDOWS}

type
  PLongBool = ^LongBool;

{ LocaleCharsFromUnicode is a cross-platform wrapper for WideCharToMultiByte
  with an emulated implemention on non-Windows platforms. The Flags parameter
  isn't supported on non-Windows platforms }

function LocaleCharsFromUnicode(CodePage, Flags: Cardinal;
  UnicodeStr: PWideChar; UnicodeStrLen: Integer; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; DefaultChar: PAnsiChar; UsedDefaultChar: PLongBool): Integer; overload;

{$IFDEF POSIX}
function LocaleCharsFromUnicode(const LocaleName: AnsiString; Flags: Cardinal;
  UnicodeStr: PWideChar; UnicodeStrLen: Integer; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; DefaultChar: PAnsiChar; UsedDefaultChar: PLongBool): Integer; overload;
{$ENDIF}

{ UnicodeFromLocaleChars is a cross-platform wrapper for MultiByteToWideChar
  with an emulated implemention on non-Windows platforms. The Flags parameter
  only supports MB_ERR_INVALID_CHARS on non-Windows platforms }

function UnicodeFromLocaleChars(CodePage, Flags: Cardinal; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; UnicodeStr: PWideChar; UnicodeStrLen: Integer): Integer; overload;

{$IFDEF POSIX}
function UnicodeFromLocaleChars(const LocaleName: AnsiString; Flags: Cardinal;
  LocaleStr: PAnsiChar; LocaleStrLen: Integer; UnicodeStr: PWideChar;
  UnicodeStrLen: Integer): Integer; overload;
{$ENDIF}

{$IFDEF POSIX}
{ GetACP returns the equivalent Windows codepage identifier based on the
  current LANG environment variable (Linux) or CFLocaleGetIdentifier (Mac OS) }

function GetACP: Cardinal;

{ Compatibility consts for LocaleCharsFromUnicode/UnicodeFromLocaleChars }

const
  CP_ACP  = 0;
  CP_UTF7 = 65000;
  CP_UTF8 = 65001;

{$ENDIF}


(* =================================================================== *)

{$IFDEF DUNIT_HELPER_TESTS}
{$INCLUDE 'DUnitSystemIntf.inc'}
{$ENDIF}

implementation

uses
  SysInit;

{$IFDEF DUNIT_HELPER_TESTS}
{$INCLUDE 'DUnitSystemImpl.inc'}
{$ENDIF}

{$IFDEF POSIX}
{$I PosixAPIs.inc }
{$ENDIF POSIX}

{$IFDEF MACOS}
{$I CoreServicesAPIs.inc }
{$I CoreFoundationAPIs.inc }
{$ENDIF MACOS}

{$IFDEF LINUX}
const
  MAX_PATH = 1024;
{$ENDIF LINUX}

{$IFDEF MACOS}
const
  MAX_PATH = 1024;
{$ENDIF MACOS}

{$IFDEF MSWINDOWS}
// for use of WindowsAPIs.inc
type
  HWND = THandle;               { from windef.h }
  HKEY = THandle;               { from windef.h }
{$ENDIF}

type
  PStrRec = ^StrRec;
  StrRec = packed record
    codePage: Word;
    elemSize: Word;
    refCnt: Longint;
    length: Longint;
  end;

const
  skew = SizeOf(StrRec);
  rOff = SizeOf(StrRec); { codePage offset }
  overHead = SizeOf(StrRec) + SizeOf(Char);
  CP_UTF16 = 1200;
{$IFDEF MSWINDOWS}
  CP_ACP  = 0;
  CP_UTF8 = 65001;
{$ENDIF MSWINDOWS}

type
  PDynArrayRec = ^TDynArrayRec;
  TDynArrayRec = packed record
    RefCnt: LongInt;
    Length: LongInt;
  end;

const
  STATUS_WAIT_0 = Cardinal($00000000);
  WAIT_OBJECT_0 = (STATUS_WAIT_0 + 0);
  ObjCastGUID: TGUID = '{CEDF24DE-80A4-447D-8C75-EB871DC121FD}';

{ This procedure should be at the very beginning of the }
{ text segment. It used to be used by _RunError to find    }
{ start address of the text segment, but is not used anymore.  }

procedure TextStart;
begin
end;


{$IFDEF PIC}
function GetGOT: Pointer; export;
begin
  asm
        MOV Result,EBX
  end;
end;
{$ENDIF}

{$IFDEF PC_MAPPED_EXCEPTIONS}
const
  UNWINDFI_TOPOFSTACK =   $BE00EF00;

type
  UNWINDPROC  = Pointer;

{$IFDEF MSWINDOWS}
const
  unwind = 'unwind.dll';

function UnwindRegisterIPLookup(fn: UNWINDPROC; StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool; cdecl;
  external unwind name '__BorUnwind_RegisterIPLookup';

function UnwindDelphiLookup(Addr: LongInt; Context: Pointer): UNWINDPROC; cdecl;
  external unwind name '__BorUnwind_DelphiLookup';

function UnwindRaiseException(Exc: Pointer): LongBool; cdecl;
  external unwind name '__BorUnwind_RaiseException';

function UnwindClosestHandler(Context: Pointer): LongWord; cdecl;
  external unwind name '__BorUnwind_ClosestDelphiHandler';
{$ENDIF}
{$IF defined(LINUX) or defined(MACOS)}
{$IFDEF LINUX}
const
  unwind = 'libcgunwind.so.1';
{$ENDIF LINUX}
{$IFDEF MACOS}
const
  unwind = 'libcgunwind.1.0.dylib';
{$ENDIF MACOS}
//{$DEFINE STATIC_UNWIND}

{$IFDEF STATIC_UNWIND}
function _BorUnwind_RegisterIPLookup(fn: UNWINDPROC; StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool; cdecl;
  external;

procedure _BorUnwind_UnregisterIPLookup(StartAddr: LongInt); cdecl;  external;

function _BorUnwind_DelphiLookup(Addr: LongInt; Context: Pointer): UNWINDPROC; cdecl;  external;

function _BorUnwind_RaiseException(Exc: Pointer): LongBool; cdecl;  external;

//function _BorUnwind_AddressIsInPCMap(Addr: LongInt): LongBool; cdecl; external;
function _BorUnwind_ClosestDelphiHandler(Context: Pointer): LongWord; cdecl; external;
{$ELSE !STATIC_UNWIND}

function _BorUnwind_RegisterIPLookup(fn: UNWINDPROC; StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool; cdecl;
  external unwind name _PU + '_BorUnwind_RegisterIPLookup';

procedure _BorUnwind_UnregisterIPLookup(StartAddr: LongInt); cdecl;
  external unwind name _PU + '_BorUnwind_UnregisterIPLookup';

function _BorUnwind_DelphiLookup(Addr: LongInt; Context: Pointer): UNWINDPROC; cdecl;
  external unwind name _PU + '_BorUnwind_DelphiLookup';

function _BorUnwind_RaiseException(Exc: Pointer): LongBool; cdecl;
  external unwind name _PU + '_BorUnwind_RaiseException';

function _BorUnwind_ClosestDelphiHandler(Context: Pointer): LongWord; cdecl;
  external unwind name _PU + '_BorUnwind_ClosestDelphiHandler';
{$ENDIF !STATIC_UNWIND}
{$IFEND LINUX or MACOS}
{$ENDIF PC_MAPPED_EXCEPTIONS}

const { copied from xx.h }
  cContinuable        = 0;
  cNonContinuable     = 1;
  cUnwinding          = 2;
  cUnwindingForExit   = 4;
  cUnwindInProgress   = cUnwinding or cUnwindingForExit;
  cDelphiException    = $0EEDFADE;
  cDelphiReRaise      = $0EEDFADF;
  cDelphiExcept       = $0EEDFAE0;
  cDelphiFinally      = $0EEDFAE1;
  cDelphiTerminate    = $0EEDFAE2;
  cDelphiUnhandled    = $0EEDFAE3;
  cNonDelphiException = $0EEDFAE4;
  cDelphiExitFinally  = $0EEDFAE5;
  cCppException       = $0EEFFACE; { used by BCB }
  EXCEPTION_CONTINUE_SEARCH    = 0;
  EXCEPTION_EXECUTE_HANDLER    = 1;
  EXCEPTION_CONTINUE_EXECUTION = -1;

{$IFDEF PC_MAPPED_EXCEPTIONS}
const
  excIsBeingHandled     = $00000001;
  excIsBeingReRaised    = $00000002;
{$ENDIF}

{$IF defined(CPU386) and not defined(PC_MAPPED_EXCEPTIONS)}
type
  JmpInstruction =
  packed record
    opCode:   Byte;
    distance: Longint;
  end;
{$IFEND CPU386 and !PC_MAPPED_EXCEPTIONS}

{$IFDEF CPU386}
type
  PExcDescEntry = ^TExcDescEntry;
  TExcDescEntry = record
    vTable:  Pointer;
    handler: Pointer;
  end;
  PExcDesc = ^TExcDesc;
  TExcDesc = packed record
{$IFNDEF PC_MAPPED_EXCEPTIONS}
    jmp: JmpInstruction;
{$ENDIF}
    case Integer of
    0:      (instructions: array [0..0] of Byte);
    1{...}: (cnt: Integer; excTab: array [0..0{cnt-1}] of TExcDescEntry);
  end;
{$ENDIF}

{$IF (not defined(CPU386)) and defined(MSWINDOWS)}
// Language specific exception data
type
  PExcDescEntry = ^TExcDescEntry;
  TExcDescEntry = record
    VTable:  LongWord; // 32 bit RVA
    Handler: LongWord; // 32 bit RVA
  end;
  PExcDesc = ^TExcDesc;
  TExcDesc = record
    DescCount: Integer;
    DescTable: array [0..0{DescCount-1}] of TExcDescEntry;
  end;
  PExcScope = ^TExcScope;
  TExcScope = record
    BeginOffset:  LongWord;  // 32 bit RVA
    EndOffset:    LongWord;  // 32 bit RVA
    TableOffset:  LongWord;  // 32 bit RVA. 0:TargetOffset=finally block
                             //             1:TargetOffset=catch block
                             //             other:TargetOffset=TExcDesc
    TargetOffset: LongWord;  // 32 bit RVA. start of finally/catch block. 0 if TableOffset > 1
                             // signature is _TDelphiFinallyHandlerProc
  end;
  PExcData = ^TExcData;
  TExcData = record
    ScopeCount: Integer;
    ScopeTable: array [0..0{ScopeCount-1}] of TExcScope;
  end;
  PFinallyExcScope = PExcScope;
  TFinallyExcScope = TExcScope;
  PFinallyExcDesc = PExcDesc;
  TFinallyExcDesc = TExcDesc;
{$IFEND}

{$IFDEF PC_MAPPED_EXCEPTIONS}
const
  UW_EXC_CLASS_BORLANDCPP = $FBEE0001;
  UW_EXC_CLASS_BORLANDDELPHI = $FBEE0101;

type
  // The following _Unwind_* types represent unwind.h
  _Unwind_Word = LongWord;
  _Unwind_Exception_Cleanup_Fn = Pointer;
  _Unwind_Exception = packed record
    exception_class: _Unwind_Word;
    exception_cleanup: _Unwind_Exception_Cleanup_Fn;
    private_1: _Unwind_Word;
    private_2: _Unwind_Word;
  end;
{$ELSE}
  PExcFrame = ^TExcFrame;
  TExcFrame = record
    next: PExcFrame;
    desc: PExcDesc;
    hEBP: Pointer;
    case Integer of
    0:  ( );
    1:  ( ConstructedObject: Pointer );
    2:  ( SelfOfMethod: Pointer );
  end;

  PRaiseFrame = ^TRaiseFrame;
  TRaiseFrame = packed record
    NextRaise: PRaiseFrame;
    ExceptAddr: Pointer;
    ExceptObject: TObject;
    ExceptionRecord: PExceptionRecord;
  end;
{$ENDIF}

const
  cCR = $0D;
  cLF = $0A;
  cEOF = $1A;


{$IFDEF POSIX}
function GetLastError: Integer;
begin
  Result := __error^;
end;

procedure SetLastError(ErrorCode: Integer);
begin
  __error^ := ErrorCode;
end;
{$ENDIF POSIX}

{$IF (not defined(CPU386)) and defined(MSWINDOWS)}

function InterlockedExchangeAdd(var Addend: Integer; Increment: Integer): Integer; stdcall;
begin
  Result := Addend;
  Inc(Addend, Increment);
end;


function InterlockedDecrement(var Addend: LongInt): LongInt; stdcall;
begin
  Dec(Addend);
  Result := Addend;
end;


function InterlockedIncrement(var Addend: LongInt): LongInt; stdcall;
begin
  Inc(Addend);
  Result := Addend;
end;


function InterlockedCompareExchange(var Destination: LongWord; Exchange: LongWord; Comparand: LongWord): LongWord; overload; stdcall;
begin
  Result := Destination;
  if Result = Comparand then Destination := Exchange;
end;


function InterlockedCompareExchange(Destination: Pointer; Exchange: LongWord; Comparand: LongWord): LongWord; overload; stdcall;
begin
  Result := PLongWord(Destination)^;
  if Result = Comparand then PLongWord(Destination)^ := Exchange;
end;


function InterlockedCompareExchange(var Destination: LongInt; Exchange: LongInt; Comparand: LongInt): LongInt; overload; stdcall;
begin
  Result := Destination;
  if Result = Comparand then Destination := Exchange;
end;


function InterlockedCompareExchangePointer(var Destination: Pointer; Exchange: Pointer; Comparand: Pointer): Pointer; stdcall;
begin
  Result := Destination;
  if Result = Comparand then Destination := Exchange;
end;

function InterlockedAdd(var Addend: Integer; Increment: Integer): Integer;
begin
  Result := InterlockedExchangeAdd(Addend, Increment);
  Result := Result + Increment;
end;


function InterlockedExchangePtr(var Dest: Pointer; Value: Pointer): Pointer;
begin
  Result := Dest;
  repeat
    Result := InterlockedCompareExchangePointer(Dest, Value, Result);
  until Dest = Value;
end;


function InterlockedExchangePointer(var Dest: Pointer; Value: Pointer): Pointer;
begin
  Result := Dest;
  repeat
    Result := InterlockedCompareExchangePointer(Dest, Value, Result);
  until Dest = Value;
end;


function InterlockedExchange(var Dest: LongInt; Value: LongInt): LongInt; overload;
begin
  Result := Dest;
  repeat
    Result := InterlockedCompareExchange(Dest, Value, Result);
  until Dest = Value;
end;


function InterlockedExchange(var Dest: Pointer; Value: Pointer): Pointer; overload;
begin
  Result := Dest;
  repeat
    Result := InterlockedCompareExchangePointer(Dest, Value, Result);
  until Dest = Value;
end;

{$IFEND (not CPU386) and MSWINDOWS}


{$IF defined(CPU386)}

function InterlockedExchangePointer(var P: Pointer; V: Pointer): Pointer;
asm
        MOV     ECX,EAX
        MOV     EAX,[ECX]
@@loop:
   LOCK CMPXCHG [ECX],EDX
        JNZ     @@loop
end;

function InterlockedExchange(var P: Pointer; V: Pointer): Pointer; overload;
asm
      JMP InterlockedExchangePointer
end;

function InterlockedExchange(var P: Longint; V: Longint): LongInt; overload;
asm
      JMP InterlockedExchangePointer
end;

function InterlockedAdd(var Addend: Integer; Increment: Integer): Integer;
asm
      MOV   ECX,EAX
      MOV   EAX,EDX
 LOCK XADD  [ECX],EAX
      ADD   EAX,EDX
end;

function InterlockedIncrement(var Addend: Integer): Integer;
asm
      MOV   EDX,1
      JMP   InterlockedAdd
end;

function InterlockedDecrement(var Addend: Integer): Integer;
asm
      MOV   EDX,-1
      JMP   InterlockedAdd
end;

function InterlockedCompareExchange(var Destination: Integer; Exchange: Integer; Comparand: Integer): Integer;
asm
      XCHG    EAX,ECX
 LOCK CMPXCHG [ECX],EDX
end;

function InterlockedCompareExchangePointer(var Destination: Pointer; Exchange: Pointer; Comparand: Pointer): Pointer;
asm
      JMP InterlockedCompareExchange
end;

{$IFEND CPU386}


{$IF defined(LINUX) or defined(MACOS)}
var
  ModuleCacheVersion: Cardinal = 0;

function ModuleCacheID: Cardinal;
begin
  Result := ModuleCacheVersion;
end;

procedure InvalidateModuleCache;
begin
  InterlockedIncrement(Integer(ModuleCacheVersion));
end;
{$IFEND LINUX or MACOS}

{$IFDEF MSWINDOWS}
{$I WindowsAPIs.INC}

function GetCmdShow: Integer;
var
  SI: TStartupInfo;
begin
  Result := 10;                  { SW_SHOWDEFAULT }
  SI.cb := SizeOf(TStartupInfo);
  GetStartupInfo(SI);
  if SI.dwFlags and 1 <> 0 then  { STARTF_USESHOWWINDOW }
    Result := SI.wShowWindow;
end;
{$ENDIF MSWINDOWS}

function WCharFromChar(WCharDest: PWideChar; DestChars: Integer; const CharSource: PAnsiChar; SrcBytes: Integer; CodePage: Integer): Integer; forward;
function CharFromWChar(CharDest: PAnsiChar; DestBytes: Integer; const WCharSource: PWideChar; SrcChars: Integer; CodePage: Integer): Integer; overload; forward;
function CharFromWChar(CharDest: PAnsiChar; DestBytes: Integer; const WCharSource: PWideChar; SrcChars: Integer): Integer; overload; forward;

{ ----------------------------------------------------- }
{       Memory manager                                  }
{ ----------------------------------------------------- }

{$IFDEF MSWINDOWS}
{$I GETMEM.INC }
{$ENDIF}

{$IFDEF POSIX}
function SysGetMem(Size: Integer): Pointer;
begin
  Result := __malloc(size);
end;

function SysFreeMem(P: Pointer): Integer;
begin
  __free(P);
  Result := 0;
end;

function SysReallocMem(P: Pointer; Size: Integer): Pointer;
begin
  Result := realloc(P, Size);
end;


function SysAllocMem(Size: Cardinal): Pointer;
begin
  // Alocate and zero memory for 1 item of Size bytes
  Result := calloc(1, Size);
end;

function SysRegisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  // not implemented for POSIX
  Result := False;
end;

function SysUnregisterExpectedMemoryLeak(P: Pointer): Boolean;
begin
  // not implemented for POSIX
  Result := False;
end;
{$ENDIF POSIX}

var
  MemoryManager: TMemoryManagerEx = (
    GetMem: SysGetMem;
    FreeMem: SysFreeMem;
    ReallocMem: SysReallocMem;
    AllocMem: SysAllocMem;
    RegisterExpectedmemoryLeak: SysRegisterExpectedMemoryLeak;
    UnregisterExpectedmemoryLeak: SysUnregisterExpectedMemoryLeak);





function AllocMem(Size: Cardinal): Pointer;
{$IFDEF PUREPASCAL}
begin
  if Size > 0 then
  begin
    Result := MemoryManager.AllocMem(Size);
    if Result = nil then
      Error(reOutOfMemory);
  end
  else
    Result := nil;
end;
{$ELSE}
asm
        TEST    EAX,EAX
        JZ      @@allocmemdone
{$IFDEF PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EBX
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX, [EAX].OFFSET MemoryManager.AllocMem
        POP     EAX
        CALL    EBX
        POP     EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
{$ELSE !PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    MemoryManager.AllocMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
{$ENDIF !PIC}
        TEST    EAX,EAX
        JZ      @@allocmemerror
@@allocmemdone:
        REP     RET  // Optimization for branch prediction
@@allocmemerror:
        MOV     AL,reOutOfMemory
        JMP     Error
end;
{$ENDIF}

function RegisterExpectedMemoryLeak(P: Pointer): boolean;
begin
  Result := (P <> nil) and MemoryManager.RegisterExpectedMemoryLeak(P);
end;

function UnregisterExpectedMemoryLeak(P: Pointer): boolean;
begin
  Result := (P <> nil) and MemoryManager.UnregisterExpectedMemoryLeak(P);
end;

{$IFDEF PC_MAPPED_EXCEPTIONS}
var
//  Unwinder: TUnwinder = (
//    RaiseException: UnwindRaiseException;
//    RegisterIPLookup: UnwindRegisterIPLookup;
//    UnregisterIPLookup: UnwindUnregisterIPLookup;
//    DelphiLookup: UnwindDelphiLookup);
  Unwinder: TUnwinder;

{$IFDEF STATIC_UNWIND}
{$IFDEF PIC}
{$L 'objs/arith.pic.o'}
{$L 'objs/diag.pic.o'}
{$L 'objs/delphiuw.pic.o'}
{$L 'objs/unwind.pic.o'}
{$ELSE}
{$L 'objs/arith.o'}
{$L 'objs/diag.o'}
{$L 'objs/delphiuw.o'}
{$L 'objs/unwind.o'}
{$ENDIF}
procedure Arith_RdUnsigned; external;
procedure Arith_RdSigned; external;
procedure __assert_fail; cdecl; external libc name '__assert_fail';
procedure malloc; cdecl; external libc name 'malloc';
procedure memset; cdecl; external libc name 'memset';
procedure strchr; cdecl; external libc name 'strchr';
procedure strncpy; cdecl; external libc name 'strncpy';
procedure strcpy; cdecl; external libc name 'strcpy';
procedure strcmp; cdecl; external libc name 'strcmp';
procedure printf; cdecl; external libc name 'printf';
procedure free; cdecl; external libc name 'free';
procedure getenv; cdecl; external libc name 'getenv';
procedure strtok; cdecl; external libc name 'strtok';
procedure strdup; cdecl; external libc name 'strdup';
procedure __strdup; cdecl; external libc name '__strdup';
procedure fopen; cdecl; external libc name 'fopen';
procedure fdopen; cdecl; external libc name 'fdopen';
procedure time; cdecl; external libc name 'time';
procedure ctime; cdecl; external libc name 'ctime';
procedure fclose; cdecl; external libc name 'fclose';
procedure fprintf; cdecl; external libc name 'fprintf';
procedure vfprintf; cdecl; external libc name 'vfprintf';
procedure fflush; cdecl; external libc name 'fflush';
procedure dup; cdecl; external libc name 'dup';
procedure debug_init; external;
procedure debug_print; external;
procedure debug_class_enabled; external;
procedure debug_continue; external;
{$ENDIF}
{$ENDIF}

{$IFDEF CPUX64}
function GetProcessHeap: THandle; stdcall;
  external 'kernel32.dll' name 'GetProcessHeap';
function HeapAlloc(Heap: THandle; Flags: LongWord; Size: NativeUInt): Pointer; stdcall;
  external 'kernel32.dll' name 'HeapAlloc';
function HeapReAlloc(Heap: THandle; Flags: LongWord; Ptr: Pointer; Size: NativeUInt): Pointer; stdcall;
  external 'kernel32.dll' name 'HeapReAlloc';
function HeapFree(Heap: THandle; Flags: LongWord; Ptr: Pointer): LongBool; stdcall;
  external 'kernel32.dll' name 'HeapFree';
function HeapSize(Heap: THandle; Flags: LongWord; Ptr: Pointer): NativeInt; stdcall;
  external 'kernel32.dll' name 'HeapSize';
{$ENDIF}





function _GetMem(Size: Integer): Pointer;
{$IFDEF PUREPASCAL}
{$IF defined(DEBUG) and defined(POSIX)}
var
  Signature: PLongInt;
{$IFEND}
begin
  if Size > 0 then
  begin
{$IF defined(DEBUG) and defined(POSIX)}
    Signature := PLongInt(MemoryManager.GetMem(Size + 4));
    if Signature = nil then
      Error(reOutOfMemory);
    Signature^ := 0;
    Result := Pointer(NativeInt(Signature) + 4);
{$ELSE !(DEBUG and POSIX)}
{$IFDEF CPUX64}
    Result := HeapAlloc(GetProcessHeap, 0, Size);
{$ELSE !CPUX64}
    Result := MemoryManager.GetMem(Size);
{$ENDIF !CPUX64}
    if Result = nil then
      Error(reOutOfMemory);
{$IFEND !(DEBUG and POSIX)}
  end
  else
    Result := nil;
end;
{$ELSE !PUREPASCAL}
asm //StackAlignSafe
        TEST    EAX,EAX
        JLE     @@negativeorzerosize
{$IFDEF PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EBX
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX, [EAX].OFFSET MemoryManager.GetMem
        POP     EAX
        CALL    EBX
        POP     EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
{$ELSE !PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    MemoryManager.GetMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
{$ENDIF !PIC}
        TEST    EAX,EAX
        JZ      @@getmemerror
        REP     RET // Optimization for branch prediction
@@getmemerror:
        MOV     AL, reOutOfMemory
        JMP     Error
@@negativeorzerosize:
        XOR     EAX, EAX
        DB      $F3 // REP RET
end;
{$ENDIF !PUREPASCAL}

const
  FreeMemorySignature = Longint($FBEEFBEE);





function _FreeMem(P: Pointer): Integer;
{$IFDEF PUREPASCAL}
{$IF defined(DEBUG) and defined(POSIX)}
var
  Signature: PLongInt;
{$IFEND DEBUG and POSIX}
begin
  if P <> nil then
  begin
{$IF defined(DEBUG) and defined(POSIX)}
    Signature := PLongInt(LongInt(P) - 4);
    if Signature^ <> 0 then
      Error(reInvalidPtr);
    Signature^ := FreeMemorySignature;
    Result := MemoryManager.Freemem(Pointer(Signature));
{$ELSE !(DEBUG and POSIX)}
{$IFDEF CPUX64}
    HeapFree(GetProcessHeap, 0, P);
    Result := 0;
{$ELSE !CPUX64}
    Result := MemoryManager.FreeMem(P);
{$ENDIF !CPUX64}
{$IFEND !(DEBUG and POSIX)}
    if Result <> 0 then
      Error(reInvalidPtr);
  end
  else
    Result := 0;
end;
{$ELSE !PUREPASCAL}
asm //StackAlignSafe
        TEST    EAX,EAX
        JZ      @@freememdone
{$IFDEF PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EBX
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX, [EAX].OFFSET MemoryManager.FreeMem
        POP     EAX
        CALL    EBX
        POP     EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
{$ELSE !PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    MemoryManager.FreeMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
{$ENDIF !PIC}
        TEST    EAX,EAX
        JNZ     @@freememerror
@@freememdone:
        REP     RET // Optimization for branch prediction
@@freememerror:
        MOV     AL,reInvalidPtr
        JMP     ERROR
end;
{$ENDIF !PUREPASCAL}




function _ReallocMem(var P: Pointer; NewSize: Integer): Pointer;
{$IFDEF PUREPASCAL}
{$IF defined(POSIX) and defined(DEBUG)}
var
  Temp: Pointer;
{$IFEND POSIX and DEBUG}
begin
  if P <> nil then
  begin
{$IF defined(POSIX) and defined(DEBUG) }
    Temp := Pointer(NativeInt(P) - 4);
    if NewSize > 0 then
    begin
      Temp := MemoryManager.ReallocMem(Temp, NewSize + 4);
      if Temp = nil then
        Error(reOutOfMemory);
      Result := Pointer(NativeInt(Temp) + 4);
    end
    else
    begin
      MemoryManager.FreeMem(Temp);
      Result := nil;
    end;
{$ELSE !(POSIX and DEBUG)}
    if NewSize > 0 then
    begin
{$IFDEF CPUX64}

      Result := HeapReAlloc(GetProcessHeap, 0, P, NewSize);
{$ELSE !CPUX64}
      Result := MemoryManager.ReallocMem(P, NewSize);
{$ENDIF !CPUX64}
      if Result = nil then
        Error(reOutOfMemory);
    end
    else
    begin
{$IFDEF CPUX64}

      _FreeMem(P);
{$ELSE !CPUX64}
      if MemoryManager.FreeMem(P) <> 0 then
        Error(reInvalidPtr);
{$ENDIF !CPUX64}
      Result := nil;
    end;
{$IFEND !(POSIX and DEBUG)}
    P := Result;
  end else
  begin
    Result := _GetMem(NewSize);
    P := Result;
  end;
end;
{$ELSE !PUREPASCAL}

asm
{$IFDEF PIC}
        PUSH    EBX
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX, EAX
        POP     EAX
{$ENDIF PIC}
        MOV     ECX, [EAX]
        TEST    ECX, ECX
        JE      @@alloc
        TEST    EDX, EDX
        JE      @@free
@@resize:
        PUSH    EAX
        MOV     EAX, ECX
{$IFDEF PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        MOV     EBX, [EBX].OFFSET MemoryManager.ReallocMem
        CALL    EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
{$ELSE !PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    MemoryManager.ReallocMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
{$ENDIF PIC}
        POP     ECX
        OR      EAX, EAX
        JE      @@allocError
        MOV     [ECX], EAX
{$IFDEF PIC}
        POP     EBX
{$ENDIF PIC}
        RET
@@freeError:
        MOV     AL, reInvalidPtr
        JMP     Error
@@free:
        MOV     [EAX], EDX
        MOV     EAX, ECX
{$IFDEF PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        MOV     EBX, [EBX].OFFSET MemoryManager.FreeMem
        CALL    EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
{$ELSE !PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    MemoryManager.FreeMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
{$ENDIF !PIC}
        OR      EAX, EAX
        JNE     @@freeError
{$IFDEF PIC}
        POP     EBX
{$ENDIF PIC}
        RET
@@allocError:
        MOV     AL, reOutOfMemory
        JMP     Error
@@alloc:
        TEST    EDX, EDX
        JE      @@exit
        PUSH    EAX
        MOV     EAX, EDX
{$IFDEF PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        MOV     EBX, [EBX].OFFSET MemoryManager.GetMem
        CALL    EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
{$ELSE !PIC}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    MemoryManager.GetMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
{$ENDIF !PIC}
        POP     ECX
        OR      EAX, EAX
        JE      @@allocError
        MOV     [ECX], EAX
{$IFDEF PIC}
        POP     EBX
{$ENDIF PIC}
@@exit:
end;
{$ENDIF !PUREPASCAL}


{ The default AllocMem implementation - for older memory managers that do not
  implement this themselves. }
function DefaultAllocMem(Size: Cardinal): Pointer;
begin
{$IFDEF CPUX64}
  Result := MemoryManager.GetMem(Size);
{$ELSE}
  Result := _GetMem(Size);
{$ENDIF}
  if (Result <> nil) then
    FillChar(Result^, Size, 0)
end;

{ The default (do nothing) leak registration function for backward compatibility
  with older memory managers. }
function DefaultRegisterAndUnregisterExpectedMemoryLeak(P: Pointer): boolean;
begin
  Result := False;
end;

{ Backward compatible GetMemoryManager implementation }
procedure GetMemoryManager(var MemMgr: TMemoryManager);
begin
  MemMgr.GetMem := MemoryManager.GetMem;
  MemMgr.FreeMem := MemoryManager.FreeMem;
  MemMgr.ReallocMem := MemoryManager.ReallocMem;
end;

{ Backward compatible SetMemoryManager implementation }
procedure SetMemoryManager(const MemMgr: TMemoryManager);
begin
  MemoryManager.GetMem := MemMgr.GetMem;
  MemoryManager.FreeMem := MemMgr.FreeMem;
  MemoryManager.ReallocMem := MemMgr.ReallocMem;
  MemoryManager.AllocMem := DefaultAllocMem;
  MemoryManager.RegisterExpectedMemoryLeak :=
    DefaultRegisterAndUnregisterExpectedMemoryLeak;
  MemoryManager.UnregisterExpectedMemoryLeak :=
    DefaultRegisterAndUnregisterExpectedMemoryLeak;
end;

procedure GetMemoryManager(var MemMgrEx: TMemoryManagerEx);
begin
  MemMgrEx := MemoryManager;
end;

procedure SetMemoryManager(const MemMgrEx: TMemoryManagerEx);
begin
  MemoryManager := MemMgrEx;
end;

function IsMemoryManagerSet: Boolean;
begin
  with MemoryManager do
    Result := (@GetMem <> @SysGetMem) or (@FreeMem <> @SysFreeMem) or
      (@ReallocMem <> @SysReallocMem) or (@AllocMem <> @SysAllocMem) or
      (@RegisterExpectedMemoryLeak <> @SysRegisterExpectedMemoryLeak) or
      (@UnregisterExpectedMemoryLeak <> @SysUnregisterExpectedMemoryLeak);
end;

{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure GetUnwinder(var Dest: TUnwinder);
begin
  Dest := Unwinder;
end;

procedure SetUnwinder(const NewUnwinder: TUnwinder);
begin
  Unwinder := NewUnwinder;
end;

function IsUnwinderSet: Boolean;
begin
  with Unwinder do
    Result := (@RaiseException <> @_BorUnwind_RaiseException) or
      (@RegisterIPLookup <> @_BorUnwind_RegisterIPLookup) or
      (@UnregisterIPLookup <> @_BorUnwind_UnregisterIPLookup) or
      (@DelphiLookup <> @_BorUnwind_DelphiLookup);
end;

procedure InitUnwinder;
var
  Addr: Pointer;
begin
  { We look to see if we can find a dynamic version of the unwinder.  This
    will be the case if the application used ShareExcept.pas.  If it is
    present, then we fire it up.  Otherwise, we use our static copy. }
  Addr := dlsym(0, '_BorUnwind_RegisterIPLookup');
  if Addr <> nil then
  begin
    Unwinder.RegisterIPLookup := Addr;
    Addr := dlsym(0, '_BorUnwind_UnregisterIPLookup');
    Unwinder.UnregisterIPLookup := Addr;
    Addr := dlsym(0, '_BorUnwind_RaiseException');
    Unwinder.RaiseException := Addr;
    Addr := dlsym(0, '_BorUnwind_DelphiLookup');
    Unwinder.DelphiLookup := Addr;
    Addr := dlsym(0, '_BorUnwind_ClosestDelphiHandler');
    Unwinder.ClosestHandler := Addr;
  end
  else
  begin
    dlerror;   // clear error state;  dlsym doesn't
    Unwinder.RegisterIPLookup := _BorUnwind_RegisterIPLookup;
    Unwinder.DelphiLookup := _BorUnwind_DelphiLookup;
    Unwinder.UnregisterIPLookup := _BorUnwind_UnregisterIPLookup;
    Unwinder.RaiseException := _BorUnwind_RaiseException;
    Unwinder.ClosestHandler := _BorUnwind_ClosestDelphiHandler;
  end;
end;

function SysClosestDelphiHandler(Context: Pointer): LongWord;
begin
  if not Assigned(Unwinder.ClosestHandler) then
    InitUnwinder;
  Result := Unwinder.ClosestHandler(Context);
end;

function SysRegisterIPLookup(StartAddr, EndAddr: LongInt; Context: Pointer; GOT: LongInt): LongBool;
begin
//  xxx
  if not Assigned(Unwinder.RegisterIPLookup) then
  begin
    InitUnwinder;
//    Unwinder.RegisterIPLookup := UnwindRegisterIPLookup;
//    Unwinder.DelphiLookup := UnwindDelphiLookup;
  end;
  Result := Unwinder.RegisterIPLookup(@Unwinder.DelphiLookup, StartAddr, EndAddr, Context, GOT);
end;

procedure SysUnregisterIPLookup(StartAddr: LongInt);
begin
//  if not Assigned(Unwinder.UnregisterIPLookup) then
//    Unwinder.UnregisterIPLookup := UnwindUnregisterIPLookup;
  Unwinder.UnregisterIPLookup(StartAddr);
end;

function SysRaiseException(Exc: Pointer): LongBool; export;
var
  uexc: _Unwind_Exception;
begin
  uexc.exception_class := UW_EXC_CLASS_BORLANDDELPHI;
  uexc.private_1 := _Unwind_Word(Exc);
  uexc.private_2 := 0;
  Result := Unwinder.RaiseException(@uexc);
end;

//  SysRaiseCPPException
//    Called to reraise a C++ exception that is unwinding through pascal code.
function SysRaiseCPPException(Exc: Pointer; priv2: Pointer; cls: LongWord): LongBool;
var
  uexc: _Unwind_Exception;
begin
  uexc.exception_class := cls;
  uexc.private_1 := _Unwind_Word(Exc);
  uexc.private_2 := _Unwind_Word(priv2);
  Result := Unwinder.RaiseException(@uexc);
end;

const
  MAX_NESTED_EXCEPTIONS = 16;
{$ENDIF PC_MAPPED_EXCEPTIONS}


{$IFDEF CPUX64}
var
{$ELSE}
threadvar
{$ENDIF}
{$IFDEF PC_MAPPED_EXCEPTIONS}
  ExceptionObjects: array[0..MAX_NESTED_EXCEPTIONS-1] of TRaisedException;
  ExceptionObjectCount: Integer;
  OSExceptionsBlocked: Integer;
  ExceptionList: PRaisedException;
{$ELSE !PC_MAPPED_EXCEPTIONS}
  RaiseListPtr: pointer;
{$ENDIF !PC_MAPPED_EXCEPTIONS}


{$IFDEF CPUX64}
var
{$ELSE}
threadvar
{$ENDIF}
  InOutRes: Integer;

{$IFDEF PUREPASCAL}
var
  notimpl: array [0..15] of Char = 'not implemented'#10;

procedure NotImplemented;
{$IFDEF MSWINDOWS}
var
  Dummy: Cardinal;
begin
  WriteFile(GetStdHandle(STD_ERROR_HANDLE), notimpl, 16, Dummy, nil);
  Halt;
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  __write (2, @notimpl, 16);
  Halt;
end;
{$ENDIF POSIX}
{$ENDIF PUREPASCAL}


{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure BlockOSExceptions;
asm  //StackAlignSafe
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        CALL    SysInit.@GetTLS
        MOV     [EAX].OSExceptionsBlocked, 1
        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
end;



procedure UnblockOSExceptions;
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    SysInit.@GetTLS
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        MOV     [EAX].OSExceptionsBlocked, 0
end;



// Access to a TLS variable.  Note the comment in BeginThread before
// you change the implementation of this function.
function AreOSExceptionsBlocked: Boolean;
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    SysInit.@GetTLS
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        MOV     EAX, [EAX].OSExceptionsBlocked
end;

const
  TRAISEDEXCEPTION_SIZE = SizeOf(TRaisedException);



function CurrentException: PRaisedException;
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    SysInit.@GetTLS
        LEA     EDX, [EAX].ExceptionObjects
        MOV     EAX, [EAX].ExceptionObjectCount
        OR      EAX, EAX
        JE      @@Done
        DEC     EAX
        IMUL    EAX, TRAISEDEXCEPTION_SIZE
        ADD     EAX, EDX
        JMP     @@Exit
@@Done:
        CALL    SysInit.@GetTLS
        MOV     EAX,[EAX].ExceptionList
@@Exit:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
end;



function CurrentPrivateException: PRaisedException;
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    SysInit.@GetTLS
        LEA     EDX, [EAX].ExceptionObjects
        MOV     EAX, [EAX].ExceptionObjectCount
        OR      EAX, EAX
        JE      @@Done
        DEC     EAX
        IMUL    EAX, TRAISEDEXCEPTION_SIZE
        ADD     EAX, EDX
@@Done:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
end;

{ In the interests of code size here, this function is slightly overloaded.
  It is responsible for freeing up the current exception record on the
  exception stack, and it conditionally returns the thrown object to the
  caller.  If the object has been acquired through AcquireExceptionObject,
  we don't return the thrown object. }



function FreeException: Pointer;
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    CurrentPrivateException
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        OR      EAX, EAX
        JE      @@Error
        { EAX -> the TRaisedException }
        XOR     ECX, ECX
        { If the exception object has been referenced, we don't return it. }
        CMP     [EAX].TRaisedException.RefCount, 0
        JA      @@GotObject
        MOV     ECX, [EAX].TRaisedException.ExceptObject
@@GotObject:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    ECX
        CALL    SysInit.@GetTLS
        POP     ECX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        DEC     [EAX].ExceptionObjectCount
        MOV     EAX, ECX
        RET
@@Error:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    SysInit.@GetTLS
        MOV     EAX, [EAX].ExceptionList
        CALL    [EAX].TRaisedException.Cleanup
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        RET
end;

procedure ReleaseDelphiException;
begin
  FreeException;
end;




function AllocateException(Exception: Pointer; ExceptionAddr: Pointer): PRaisedException;
asm
        PUSH    EBX // This is to simplify stack aligment for PIC and non PIC.
        PUSH    EAX
        PUSH    EDX
{$IFDEF PIC}
        CALL    GetGOT
        MOV     EBX, EAX
{$ELSE !PIC}
        XOR     EBX, EBX
{$ENDIF !PIC}
        CALL    SysInit.@GetTLS
        CMP     [EAX].ExceptionObjectCount, MAX_NESTED_EXCEPTIONS-1
        JE      @@TooManyNestedExceptions
        INC     [EAX].ExceptionObjectCount
        CALL    CurrentException
        POP     EDX
        POP     ECX
        MOV     [EAX].TRaisedException.ExceptObject, ECX
        MOV     [EAX].TRaisedException.ExceptionAddr, EDX
        MOV     [EAX].TRaisedException.RefCount, 0
        MOV     [EAX].TRaisedException.HandlerEBP, $FFFFFFFF
        MOV     [EAX].TRaisedException.Flags, 0
        MOV     [EAX].TRaisedException.Prev, 0
        LEA     EDX, [EBX].OFFSET FreeException
        MOV     [EAX].TRaisedException.Cleanup, EDX
        LEA     EDX, [EBX].OFFSET ReleaseDelphiException
        MOV     [EAX].TRaisedException.ReleaseProc, EDX
        POP     EBX
        RET
@@TooManyNestedExceptions:
        ADD     ESP, 12  // Throw away EDX, EBX and EAX.
        MOV     EAX, 231
        JMP     _RunError
end;



function AcquireExceptionObject: Pointer;
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    CurrentException
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        OR      EAX, EAX
        JE      @@Error
        INC     [EAX].TRaisedException.RefCount
        MOV     EAX, [EAX].TRaisedException.ExceptObject
        RET
@@Error:
   RET // windows version doesn't generate an error, and Halt0 calls this always
        { This happens if there is no exception pending }
//        JMP     _Run0Error
end;



procedure ReleaseExceptionObject;
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    CurrentException
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        OR      EAX, EAX
        JE      @@Error
        CMP     [EAX].TRaisedException.RefCount, 0
        JE      @@Error
        DEC     [EAX].TRaisedException.RefCount
        RET
@@Error:
{ This happens if there is no exception pending, or
  if the reference count on a pending exception is
  zero. }
        JMP   _Run0Error
end;

function ExceptObject: TObject;
var
  Exc: PRaisedException;
begin
  Exc := CurrentException;
  if Exc <> nil then
    Result := TObject(Exc^.ExceptObject)
  else
    Result := nil;
end;

{ Return current exception address }
function ExceptAddr: Pointer;
var
  Exc: PRaisedException;
begin
  Exc := CurrentException;
  if Exc <> nil then
    Result := Exc^.ExceptionAddr
  else
    Result := nil;
end;
{$ELSE !PC_MAPPED_EXCEPTIONS}  {not PC_MAPPED_EXCEPTIONS}

function ExceptObject: TObject;
begin
  if RaiseListPtr <> nil then
    Result := PRaiseFrame(RaiseListPtr)^.ExceptObject
  else
    Result := nil;
end;

{ Return current exception address }
function ExceptAddr: Pointer;
begin
  if RaiseListPtr <> nil then
    Result := PRaiseFrame(RaiseListPtr)^.ExceptAddr
  else
    Result := nil;
end;

function AcquireExceptionObject: Pointer;
type
  ExceptionAcquiredProc = procedure (Obj: Pointer);
var
  RaiseFrame: PRaiseFrame;
begin
  RaiseFrame := RaiseListPtr;
  if RaiseFrame <> nil then
  begin
    Result := RaiseFrame^.ExceptObject;
    RaiseFrame^.ExceptObject := nil;
    if Assigned(ExceptionAcquired) then
      ExceptionAcquiredProc(ExceptionAcquired)(Result);
  end
  else
    Result := nil;
end;

procedure ReleaseExceptionObject;
begin
end;

function RaiseList: Pointer;
begin
  Result := RaiseListPtr;
end;




function SetRaiseList(NewPtr: Pointer): Pointer;
{$IF not defined(CPU386)}
begin
  Result := InterlockedExchangePointer(RaiseListPtr, NewPtr);
end;
{$ELSE}
asm
        PUSH    EAX
{$IFDEF ALIGN_STACK}
        SUB    ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    SysInit.@GetTLS
{$IFDEF ALIGN_STACK}
        ADD    ESP, 8
{$ENDIF ALIGN_STACK}
        MOV     EDX, [EAX].RaiseListPtr
        POP     [EAX].RaiseListPtr
        MOV     EAX, EDX
end;
{$IFEND CPU386}
{$ENDIF !PC_MAPPED_EXCEPTIONS}

{
  Coverage helper glue - just go directly to the external coverage
  library.  NEVER put code in here, because we sometimes want to run
  coverage analysis on the System unit.
}

procedure _CVR_PROBE; external 'coverage.dll' name '__CVR_PROBE';
function _CVR_STMTPROBE; external 'coverage.dll' name '__CVR_STMTPROBE';

{ ----------------------------------------------------- }
{    local functions & procedures of the system unit    }
{ ----------------------------------------------------- }

procedure RunErrorAt(ErrCode: Integer; ErrorAtAddr: Pointer);
begin
  ErrorAddr := ErrorAtAddr;
  _Halt(ErrCode);
end;

procedure ErrorAt(ErrorCode: Byte; ErrorAddr: Pointer);

const
  reMap: array [TRunTimeError] of Byte = (
    0,   { reNone }
    203, { reOutOfMemory }
    204, { reInvalidPtr }
    200, { reDivByZero }
    201, { reRangeError }
{   210    Abstract error }
    215, { reIntOverflow }
    207, { reInvalidOp }
    200, { reZeroDivide }
    205, { reOverflow }
    206, { reUnderflow }
    219, { reInvalidCast }
    216, { reAccessViolation }
    218, { rePrivInstruction }
    217, { reControlBreak }
    202, { reStackOverflow }
    220, { reVarTypeCast }
    221, { reVarInvalidOp }
    222, { reVarDispatch }
    223, { reVarArrayCreate }
    224, { reVarNotArray }
    225, { reVarArrayBounds }
{   226    Thread init failure }
    227, { reAssertionFailed }
    0,   { reExternalException not used here; in SysUtils }
    228, { reIntfCastError }
    229, { reSafeCallError }
    235, { reMonitorNotLocked }
    236  { reNoMonitorSupport }
{$IFDEF PC_MAPPED_EXCEPTIONS}
{   230   Reserved by the compiler for unhandled exceptions }
{   231   Too many nested exceptions }
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IF Defined(LINUX) or Defined(MACOS)}
{   232   Fatal signal raised on a non-Delphi thread }
    , 233 { reQuit }
{$IFEND LINUX or MACOS}
{$IFDEF POSIX}
    , 234  { reCodesetConversion }
{$ENDIF POSIX}
    , 237 { rePlatformNotImplemented }
);

begin
  errorCode := errorCode and 127;
  if Assigned(ErrorProc) then
    ErrorProc(errorCode, ErrorAddr);
  if errorCode = 0 then
    errorCode := InOutRes
  else if errorCode <= Byte(High(TRuntimeError)) then
    errorCode := reMap[TRunTimeError(errorCode)];
  RunErrorAt(errorCode, ErrorAddr);
end;





{$IFOPT O+}
  // Turn off optimizations to force creating a EBP stack frame and
  // place params on the stack.
  {$DEFINE OPTIMIZATIONSON}
  {$O-}
{$ENDIF}
procedure Error(errorCode: TRuntimeError);
begin
  // Stack looks like this:
  // [EBP + 4] : Return address
  // [EBP]     : Stack Frame
  // [EBP - 1] : errorCode
  ErrorAt(Byte(errorCode) and 127,
    PPointer(Integer(@errorCode) + SizeOf(errorCode) + SizeOf(Pointer))^);
end;
{$IFDEF OPTIMIZATIONSON}
  {$UNDEF OPTIMIZATIONSON}
  {$O+}
{$ENDIF}

procedure SetLineBreakStyle(var T: Text; Style: TTextLineBreakStyle);
begin
  if TTextRec(T).Mode = fmClosed then
    TTextRec(T).Flags := (TTextRec(T).Flags and not tfCRLF) or (tfCRLF * Byte(Style))
  else
    SetInOutRes(107);  // can't change mode of open file
end;




procedure __IOTest;
{$IF not defined(CPU386)}
begin
  if InOutRes <> 0 then
    Error(reNone);
end;
{$ELSE}
asm
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX
        CALL    SysInit.@GetTLS
        CMP     [EAX].InOutRes,0
        POP     ECX
        POP     EDX
        POP     EAX
        JNE     @error
        RET
@error:
        XOR     EAX,EAX
        JMP     Error
end;
{$IFEND}

procedure SetInOutRes(NewValue: Integer);
begin
  InOutRes := NewValue;
end;

procedure InOutError;
begin
  SetInOutRes(GetLastError);
end;

procedure ChDir(const S: string);
begin
  // U-OK
  ChDir(PChar(S));
end;

procedure ChDir(P: PChar);
{$IFDEF MSWINDOWS}
begin
  // U-OK
  if not SetCurrentDirectory(P) then
    InOutError;
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
   us: UTF8String;
begin
  // U-OK
  us := UTF8String(P);
  if __chdir(PAnsiChar(us)) <> 0 then
    InOutError;
end;
{$ENDIF POSIX}

procedure _UGetDir(D: Byte; var S: UnicodeString);
{$IFDEF MSWINDOWS}
var
  Drive: array[0..3] of WideChar;
  DirBuf, SaveBuf: array[0..MAX_PATH] of WideChar;
begin
  if D <> 0 then
  begin
    Drive[0] := WideChar(D + Ord('A') - 1);
    Drive[1] := ':';
    Drive[2] := #0;
    GetCurrentDirectoryW(SizeOf(SaveBuf) div SizeOf(WideChar), SaveBuf);
    SetCurrentDirectoryW(Drive);
  end;
  GetCurrentDirectoryW(SizeOf(DirBuf) div SizeOf(WideChar), DirBuf);
  if D <> 0 then SetCurrentDirectoryW(SaveBuf);
  S := DirBuf;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  DirBuf: array[0..MAX_PATH] of AnsiChar;
begin
  getcwd(DirBuf, sizeof(DirBuf));
  S := UTF8ToString(DirBuf);
{$ENDIF POSIX}
end;

procedure _LGetDir(D: Byte; var S: AnsiString);
{$IFDEF MSWINDOWS}
var
  Drive: array[0..3] of AnsiChar;
  DirBuf, SaveBuf: array[0..MAX_PATH] of AnsiChar;
begin
  if D <> 0 then
  begin
    Drive[0] := AnsiChar(Chr(D + Ord('A') - 1));
    Drive[1] := ':';
    Drive[2] := #0;
    GetCurrentDirectoryA(SizeOf(SaveBuf), SaveBuf);
    SetCurrentDirectoryA(Drive);
  end;
  GetCurrentDirectoryA(SizeOf(DirBuf), DirBuf);
  if D <> 0 then SetCurrentDirectoryA(SaveBuf);
  S := DirBuf;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  DirBuf: array[0..MAX_PATH] of AnsiChar;
begin
  getcwd(DirBuf, sizeof(DirBuf));
  S := AnsiString(UTF8ToString(DirBuf));
{$ENDIF POSIX}
end;

procedure _WGetDir(D: Byte; var S: WideString);
{$IFDEF MSWINDOWS}
var
  Drive: array[0..3] of WideChar;
  DirBuf, SaveBuf: array[0..MAX_PATH] of WideChar;
begin
  if D <> 0 then
  begin
    Drive[0] := WideChar(Chr(D + Ord('A') - 1));
    Drive[1] := ':';
    Drive[2] := #0;
    GetCurrentDirectoryW(Length(SaveBuf), SaveBuf);
    SetCurrentDirectoryW(Drive);
  end;
  GetCurrentDirectoryW(Length(DirBuf), DirBuf);
  if D <> 0 then SetCurrentDirectoryW(SaveBuf);
  S := DirBuf;
{$ENDIF MSWINDOWS}
{$IF defined(LINUX) or defined(MACOS)}
var
   U: UnicodeString;
begin
   _UGetDir(D, U);
   S := U;
{$IFEND}
end;

procedure _SGetDir(D: Byte; var S: ShortString);
var
  L: AnsiString;
begin
  _LGetDir(D, L);
  S := L;
end;

function IOResult: Integer;
begin
  Result := InOutRes;
  InOutRes := 0;
end;

procedure MkDir(const S: string);
begin
  MkDir(PChar(s));
end;

procedure MkDir(P: PChar);
begin
{$IFDEF MSWINDOWS}
  if not CreateDirectory(P, 0) then
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  if __mkdir(PAnsiChar(UTF8Encode(P)), mode_t(-1)) <> 0 then
{$ENDIF POSIX}
    InOutError;
end;







(* ***** BEGIN LICENSE BLOCK *****
 *
 * The assembly function Move is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): John O'Harrow
 *
 * ***** END LICENSE BLOCK ***** *)
procedure Move(const Source; var Dest; Count: Integer);
{$IF (not defined(CPU386))}
var
  S, D: PByte;
  I: Integer;
begin
  S := PByte(@Source);
  D := PByte(@Dest);
  if S = D then Exit;
  if NativeUInt(D) > NativeUInt(S) then
    for I := Count-1 downto 0 do
      D[I] := S[I]
  else
    for I := 0 to Count-1 do
      D[I] := S[I];
end;
{$ELSE CPU386}
asm
        CMP     EAX, EDX
        JE      @@Exit {Source = Dest}
        CMP     ECX, 32
        JA      @@LargeMove {Count > 32 or Count < 0}
        SUB     ECX, 8
        JG      @@SmallMove
@@TinyMove: {0..8 Byte Move}
{$IFDEF PIC}
        PUSH    EBX
        PUSH    EAX
        PUSH    ECX
        CALL    GetGOT
        POP     ECX
        MOV     EBX, EAX
        ADD     EBX, offset @@JumpTable+32
        MOV     ECX, [EBX+ECX*4]
        ADD     ECX, EAX
        POP     EAX
        POP     EBX
        JMP     ECX
{$ELSE}
        JMP     DWORD PTR [@@JumpTable+32+ECX*4]
{$ENDIF}
@@SmallMove: {9..32 Byte Move}
        FILD    QWORD PTR [EAX+ECX] {Load Last 8}
        FILD    QWORD PTR [EAX] {Load First 8}
        CMP     ECX, 8
        JLE     @@Small16
        FILD    QWORD PTR [EAX+8] {Load Second 8}
        CMP     ECX, 16
        JLE     @@Small24
        FILD    QWORD PTR [EAX+16] {Load Third 8}
        FISTP   QWORD PTR [EDX+16] {Save Third 8}
@@Small24:
        FISTP   QWORD PTR [EDX+8] {Save Second 8}
@@Small16:
        FISTP   QWORD PTR [EDX] {Save First 8}
        FISTP   QWORD PTR [EDX+ECX] {Save Last 8}
@@Exit:
        RET
        NOP {4-Byte Align JumpTable}
        NOP
@@JumpTable: {4-Byte Aligned}
        DD      @@Exit, @@M01, @@M02, @@M03, @@M04, @@M05, @@M06, @@M07, @@M08
@@LargeForwardMove: {4-Byte Aligned}
        PUSH    EDX
        FILD    QWORD PTR [EAX] {First 8}
        LEA     EAX, [EAX+ECX-8]
        LEA     ECX, [ECX+EDX-8]
        FILD    QWORD PTR [EAX] {Last 8}
        PUSH    ECX
        NEG     ECX
        AND     EDX, -8 {8-Byte Align Writes}
        LEA     ECX, [ECX+EDX+8]
        POP     EDX
@FwdLoop:
        FILD    QWORD PTR [EAX+ECX]
        FISTP   QWORD PTR [EDX+ECX]
        ADD     ECX, 8
        JL      @FwdLoop
        FISTP   QWORD PTR [EDX] {Last 8}
        POP     EDX
        FISTP   QWORD PTR [EDX] {First 8}
        RET
@@LargeMove:
        JNG     @@LargeDone {Count < 0}
        CMP     EAX, EDX
        JA      @@LargeForwardMove
        SUB     EDX, ECX
        CMP     EAX, EDX
        LEA     EDX, [EDX+ECX]
        JNA     @@LargeForwardMove
        SUB     ECX, 8 {Backward Move}
        PUSH    ECX
        FILD    QWORD PTR [EAX+ECX] {Last 8}
        FILD    QWORD PTR [EAX] {First 8}
        ADD     ECX, EDX
        AND     ECX, -8 {8-Byte Align Writes}
        SUB     ECX, EDX
@BwdLoop:
        FILD    QWORD PTR [EAX+ECX]
        FISTP   QWORD PTR [EDX+ECX]
        SUB     ECX, 8
        JG      @BwdLoop
        POP     ECX
        FISTP   QWORD PTR [EDX] {First 8}
        FISTP   QWORD PTR [EDX+ECX] {Last 8}
@@LargeDone:
        RET
@@M01:
        MOVZX   ECX, [EAX]
        MOV     [EDX], CL
        RET
@@M02:
        MOVZX   ECX, WORD PTR [EAX]
        MOV     [EDX], CX
        RET
@@M03:
        MOV     CX, [EAX]
        MOV     AL, [EAX+2]
        MOV     [EDX], CX
        MOV     [EDX+2], AL
        RET
@@M04:
        MOV     ECX, [EAX]
        MOV     [EDX], ECX
        RET
@@M05:
        MOV     ECX, [EAX]
        MOV     AL, [EAX+4]
        MOV     [EDX], ECX
        MOV     [EDX+4], AL
        RET
@@M06:
        MOV     ECX, [EAX]
        MOV     AX, [EAX+4]
        MOV     [EDX], ECX
        MOV     [EDX+4], AX
        RET
@@M07:
        MOV     ECX, [EAX]
        MOV     EAX, [EAX+3]
        MOV     [EDX], ECX
        MOV     [EDX+3], EAX
        RET
@@M08:
        FILD    QWORD PTR [EAX]
        FISTP   QWORD PTR [EDX]
end;
{$IFEND CPU386}

procedure MoveChars(const Source; var Dest; Length: Integer);
begin
  Move(Source, Dest, Length * SizeOf(Char));
end;

{$IFDEF MSWINDOWS}
function GetParamStr(P: PChar; var Param: string): PChar;
var
  i, Len: Integer;
  Start, S: PChar;
begin
  // U-OK
  while True do
  begin
    while (P[0] <> #0) and (P[0] <= ' ') do
      Inc(P);
    if (P[0] = '"') and (P[1] = '"') then Inc(P, 2) else Break;
  end;
  Len := 0;
  Start := P;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        Inc(Len);
        Inc(P);
      end;
      if P[0] <> #0 then
        Inc(P);
    end
    else
    begin
      Inc(Len);
      Inc(P);
    end;
  end;

  SetLength(Param, Len);

  P := Start;
  S := Pointer(Param);
  i := 0;
  while P[0] > ' ' do
  begin
    if P[0] = '"' then
    begin
      Inc(P);
      while (P[0] <> #0) and (P[0] <> '"') do
      begin
        S[i] := P^;
        Inc(P);
        Inc(i);
      end;
      if P[0] <> #0 then Inc(P);
    end
    else
    begin
      S[i] := P^;
      Inc(P);
      Inc(i);
    end;
  end;

  Result := P;
end;
{$ENDIF}

function ParamCount: Integer;
{$IFDEF MSWINDOWS}
var
  P: PChar;
  S: string;
begin
  // U-OK
  Result := 0;
  P := GetParamStr(GetCommandLine, S);
  while True do
  begin
    P := GetParamStr(P, S);
    if S = '' then Break;
    Inc(Result);
  end;
{$ENDIF MSWINDOWS}
{$IF defined(LINUX) or defined(MACOS)}
begin
  if ArgCount > 1 then
    Result := ArgCount - 1
  else Result := 0;
{$IFEND LINUX or MACOS}
end;

type
  PAnsiCharArray = array[0..0] of PAnsiChar;

function ParamStr(Index: Integer): string;
{$IFDEF MSWINDOWS}
var
  P: PChar;
  Buffer: array[0..260] of Char;
begin
  Result := '';
  if Index = 0 then
    SetString(Result, Buffer, GetModuleFileName(0, Buffer, Length(Buffer)))
  else
  begin
    P := GetCommandLine;
    while True do
    begin
      P := GetParamStr(P, Result);
      if (Index = 0) or (Result = '') then Break;
      Dec(Index);
    end;
  end;
{$ENDIF MSWINDOWS}
{$IF defined(LINUX) or defined(MACOS)}
begin
  if Index < ArgCount then
    Result := string(PAnsiCharArray(ArgValues^)[Index])
  else
    Result := '';
{$IFEND LINUX or MACOS}
end;

procedure Randomize;
{$IFDEF MSWINDOWS}
var
  Counter: Int64;
begin
  if QueryPerformanceCounter(Counter) then
    RandSeed := Counter
  else
    RandSeed := GetTickCount;
end;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
var
  TimeOfDay: timeval;
begin
  if gettimeofday(TimeOfDay, nil) = 0 then
    RandSeed := TimeOfDay.tv_sec * 1000000 + TimeOfDay.tv_usec
  else
    RandSeed := time(nil);
end;
{$ENDIF LINUX}
{$IFDEF MACOS}
begin
  RandSeed := AbsoluteToNanoseconds(UpTime);
end;
{$ENDIF MACOS}





function Random(const ARange: Integer): Integer;
{$IFDEF PUREPASCAL}
var
  Temp: LongInt;
begin
  Temp := RandSeed * $08088405 + 1;
  RandSeed := Temp;
  Result := (UInt64(Cardinal(ARange)) * UInt64(Temp)) shr 32;
end;
{$ELSE !PUREPASCAL}
asm
{     ->EAX     Range   }
{     <-EAX     Result  }
        PUSH    EBX
{$IFDEF PIC}
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX,EAX
        POP     EAX
        MOV     ECX,[EBX].RandSeed
        IMUL    EDX,[ECX],08088405H
        INC     EDX
        MOV     [ECX],EDX
{$ELSE !PIC}
        XOR     EBX, EBX
        IMUL    EDX,[EBX].RandSeed,08088405H
        INC     EDX
        MOV     [EBX].RandSeed,EDX
{$ENDIF !PIC}

        MUL     EDX
        MOV     EAX,EDX
        POP     EBX
end;
{$ENDIF !PUREPASCAL}





function Random: Extended;
const
  two2neg32: double = ((1.0/$10000) / $10000);  // 2^-32
{$IFDEF PUREPASCAL}
var
  Temp: LongInt;
  F: Extended;
begin
  Temp := RandSeed * $08088405 + 1;
  RandSeed := Temp;
  F  := Temp;
  Result := F * two2neg32;
end;
{$ELSE !PUREPASCAL}
asm
{       FUNCTION _RandExt: Extended;    }

        PUSH    EBX
{$IFDEF PIC}
        CALL    GetGOT
        MOV     EBX,EAX
        MOV     ECX,[EBX].OFFSET RandSeed
        IMUL    EDX,[ECX],08088405H
        INC     EDX
        MOV     [ECX],EDX
{$ELSE !PIC}
        XOR     EBX, EBX
        IMUL    EDX,[EBX].RandSeed,08088405H
        INC     EDX
        MOV     [EBX].RandSeed,EDX
{$ENDIF !PIC}

        FLD     [EBX].two2neg32
        PUSH    0
        PUSH    EDX
        FILD    qword ptr [ESP]
        ADD     ESP,8
        FMULP   ST(1), ST(0)
        POP     EBX
end;
{$ENDIF !PUREPASCAL}

procedure RmDir(const S: string);
begin
  // U-OK
  RmDir(PChar(s));
end;

procedure RmDir(P: PChar);
{$IFDEF POSIX}
var
   us: UTF8String;
{$ENDIF POSIX}
begin
  // U-OK
{$IFDEF MSWINDOWS}
  if not RemoveDirectory(P) then
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  us := UTF8String(P);
  if __rmdir(PAnsiChar(us)) <> 0 then
{$ENDIF POSIX}
    InOutError;
end;

function UpCase(ch: AnsiChar): AnsiChar;
begin
  Result := Ch;
  if Result in ['a'..'z'] then
    Dec(Result, Ord('a')-Ord('A'));
end;

function CharUpperA(lpsz: PAnsiChar): PAnsiChar; stdcall;
  external 'user32.dll' name 'CharUpperA';
function CharUpperW(lpwsz: PWideChar): PWideChar; stdcall;
  external 'user32.dll' name 'CharUpperW';
function CharUpper(lpwsz: PWideChar): PWideChar; stdcall;
  external 'user32.dll' name 'CharUpperW';

function UpCase(Ch: WideChar): WideChar;
begin
  Result := Ch;
  case Ch of
    'a'..'z':
      Result := WideChar(Word(Ch) and $FFDF);
  end;
end;

procedure Reset8087CW;
begin
  Set8087CW(Default8087CW);
end;



procedure Set8087CW(NewCW: Word);
{$IFDEF CPU386}
begin
  Default8087CW := NewCW;
  asm
        FNCLEX  // don't raise pending exceptions enabled by the new flags
{$IFDEF PIC}
        MOV     EAX,[EBX].OFFSET Default8087CW
        FLDCW   [EAX]
{$ELSE !PIC}
        FLDCW   Default8087CW
{$ENDIF !PIC}
  end;
end;
{$ELSE !CPU386}
begin

end;
{$ENDIF !CPU386}



function Get8087CW: Word;
{$IFDEF CPU386}
asm
        PUSH    0
        FNSTCW  [ESP].Word
        POP     EAX
end;
{$ELSE}
begin
  Result := 0;
end;
{$ENDIF}



function Int(const X: Extended): Extended;
{$IFDEF CPU386}
asm
        FLD     X
        SUB     ESP,4
        FNSTCW  [ESP].Word     // save
        FNSTCW  [ESP+2].Word   // scratch
        FWAIT
        OR      [ESP+2].Word, $0F00  // trunc toward zero, full precision
        FLDCW   [ESP+2].Word
        FRNDINT
        FWAIT
        FLDCW   [ESP].Word
        ADD     ESP,4
end;
{$ELSE}
begin

  Result := 0;
end;
{$ENDIF}




function Frac(const X: Extended): Extended;
{$IFDEF CPU386}
asm
        FLD     X
        FLD     ST(0)
        SUB     ESP,4
        FNSTCW  [ESP].Word     // save
        FNSTCW  [ESP+2].Word   // scratch
        FWAIT
        OR      [ESP+2].Word, $0F00  // trunc toward zero, full precision
        FLDCW   [ESP+2].Word
        FRNDINT
        FWAIT
        FLDCW   [ESP].Word
        ADD     ESP,4
        FSUB
end;
{$ELSE}
begin
  Result := 0;
end;
{$ENDIF}




function Exp(const X: Extended): Extended;
{$IFDEF CPU386}
asm
        {       e**x = 2**(x*log2(e))   }
        FLD     X
        FLDL2E              { y := x*log2e;      }
        FMUL
        FLD     ST(0)       { i := round(y);     }
        FRNDINT
        FSUB    ST(1), ST   { f := y - i;        }
        FXCH    ST(1)       { z := 2**f          }
        F2XM1
        FLD1
        FADD
        FSCALE              { result := z * 2**i }
        FSTP    ST(1)
end;
{$ELSE}
begin
  Result := 0;
end;
{$ENDIF}




function Cos(const X: Extended): Extended;
{$IFDEF CPU386}
asm
        FLD     X
        FCOS
        FWAIT
end;
{$ELSE}
begin
  Result := 0;
end;
{$ENDIF}




function Sin(const X: Extended): Extended;
{$IFDEF CPU386}
asm
        FLD     X
        FSIN
        FWAIT
end;
{$ELSE}
begin
  Result := 0;
end;
{$ENDIF}




function Ln(const X: Extended): Extended;
{$IFDEF CPU386}
asm
        FLD     X
        FLDLN2
        FXCH
        FYL2X
        FWAIT
end;
{$ELSE}
begin
  Result := 0;
end;
{$ENDIF}




function ArcTan(const X: Extended): Extended;
{$IFDEF CPU386}
asm
        FLD    X
        FLD1
        FPATAN
        FWAIT
end;
{$ELSE}
begin
  Result := 0;
end;
{$ENDIF}




function Sqrt(const X: Extended): Extended;
{$IFDEF CPU386}
asm
        FLD     X
        FSQRT
        FWAIT
end;
{$ELSE}
begin
  Result := 0;
end;
{$ENDIF}

{ ----------------------------------------------------- }
{       functions & procedures that need compiler magic }
{ ----------------------------------------------------- }




{$IFDEF CPU386}
procedure _ROUND;
asm
        { ->    FST(0)  Extended argument       }
        { <-    EDX:EAX Result                  }

        SUB     ESP,8
        FISTP   qword ptr [ESP]
        FWAIT
        POP     EAX
        POP     EDX
end;
{$ELSE}
function _ROUND(val: Extended): Int64;
begin
  // Extended = 64 bits
  Result := 0;
end;
{$ENDIF}




{$IFDEF CPU386}
procedure       _TRUNC;
asm
        { ->    FST(0)  Extended argument       }
        { <-    EDX:EAX Result                  }

        SUB     ESP,12
        FNSTCW  [ESP].Word          // save
        FNSTCW  [ESP+2].Word        // scratch
        FWAIT
        OR      [ESP+2].Word, $0F00  // trunc toward zero, full precision
        FLDCW   [ESP+2].Word
        FISTP   qword ptr [ESP+4]
        FWAIT
        FLDCW   [ESP].Word
        POP     ECX
        POP     EAX
        POP     EDX
end;
{$ELSE}
function _TRUNC(val: Extended): Int64;
begin
  // Extended = 64 bits
  Result := 0;
end;
{$ENDIF}





procedure _AbstractError;
{$IF (not defined(CPU386)) or defined(PIC)}
begin

  if Assigned(AbstractErrorProc) then
    AbstractErrorProc;
  _RunError(210);  // loses return address
end;
{$ELSE}
asm
        CMP     AbstractErrorProc, 0
        JE      @@NoAbstErrProc
{$IFDEF ALIGN_STACK}
        SUB    ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    AbstractErrorProc
{$IFDEF ALIGN_STACK}
        ADD    ESP, 12
{$ENDIF ALIGN_STACK}
@@NoAbstErrProc:
        MOV     EAX,210
        JMP     _RunError
end;
{$IFEND}

function TextOpen(var t: TTextRec): Integer; forward;

function OpenText(var t: TTextRec; Mode: Word): Integer;
begin
  if (t.Mode < fmClosed) or (t.Mode > fmInOut) then
    Result := 102
  else
  begin
    if t.Mode <> fmClosed then _Close(t);
    t.Mode := Mode;
    if (t.Name[0] = #0) and (t.OpenFunc = nil) then  // stdio
      t.OpenFunc := @TextOpen;
    Result := TTextIOFunc(t.OpenFunc)(t);
  end;
  if Result <> 0 then SetInOutRes(Result);
end;

function _ResetText(var t: TTextRec): Integer;
begin
  Result := OpenText(t, fmInput);
end;

function _RewritText(var t: TTextRec): Integer;
begin
  Result := OpenText(t, fmOutput);
end;

function _Append(var t: TTextRec): Integer;
begin
  Result := OpenText(t, fmInOut);
end;

function TextIn(var t: TTextRec): Integer;
const
  ERROR_BROKEN_PIPE = 109;
begin
  t.BufEnd := 0;
  t.BufPos := 0;
{$IFDEF MSWINDOWS}
  if ReadFile(t.Handle, t.BufPtr^, t.BufSize, t.BufEnd, nil) = 0 then
  begin
    Result := GetLastError;
    if Result = ERROR_BROKEN_PIPE then
      Result := 0; // NT quirk: got "broken pipe"? it's really eof
  end
  else
    Result := 0;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  t.BufEnd := __read(t.Handle, t.BufPtr, t.BufSize);
  if Integer(t.BufEnd) = -1 then
  begin
    t.BufEnd := 0;
    Result := GetLastError;
  end
  else
    Result := 0;
{$ENDIF POSIX}
end;

function FileNOPProc(var t): Integer;
begin
  Result := 0;
end;

function TextOut(var t: TTextRec): Integer;
{$IFDEF MSWINDOWS}
var
  Dummy: Cardinal;
{$ENDIF}
begin
  if t.BufPos = 0 then
    Result := 0
  else
  begin
{$IFDEF MSWINDOWS}
    if WriteFile(t.Handle, t.BufPtr^, t.BufPos, Dummy, nil) = 0 then
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
    if __write(t.Handle, t.BufPtr, t.BufPos) = ssize_t(-1) then
{$ENDIF POSIX}
      Result := GetLastError
    else
      Result := 0;
    t.BufPos := 0;
  end;
end;

function InternalClose(Handle: Integer): Boolean;
begin
{$IFDEF MSWINDOWS}
  Result := CloseHandle(Handle) = 1;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  Result := __close(Handle) = 0;
{$ENDIF POSIX}
end;

function TextClose(var t: TTextRec): Integer;
begin
  t.Mode := fmClosed;
  if not InternalClose(t.Handle) then
    Result := GetLastError
  else
    Result := 0;
end;

function TextOpenCleanup(var t: TTextRec): Integer;
begin
  InternalClose(t.Handle);
  t.Mode := fmClosed;
  Result := GetLastError;
end;

function TextOpen(var t: TTextRec): Integer;
{$IFDEF MSWINDOWS}
{$IF not defined(CPU386)}
var
  OpenMode: Integer;
  Flags, Std: Integer;
  Temp: THandle;
  I, BytesRead: Cardinal;
begin
  Result := 0;
  t.BufPos := 0;
  t.BufEnd := 0;
  case t.Mode of
    fmInput: // called by Reset
      begin
        OpenMode := GENERIC_READ;
        Flags := OPEN_EXISTING;
        t.InOutFunc := @TextIn;
      end;
    fmOutput: // called by Rewrite
      begin
        OpenMode := GENERIC_WRITE;
        Flags := CREATE_ALWAYS;
        t.InOutFunc := @TextOut;
      end;
    fmInOut:  // called by Append
      begin
        OpenMode := GENERIC_READ or GENERIC_WRITE;
        Flags := OPEN_EXISTING;
        t.InOutFunc := @TextOut;
      end;
  else
    Exit;
  end;

  t.FlushFunc := @FileNOPProc;

  if t.Name[0] = #0 then  // stdin or stdout
  begin
    if t.BufPtr = nil then  // don't overwrite bufptr provided by SetTextBuf
    begin
      t.BufPtr := @t.Buffer;
      t.BufSize := SizeOf(t.Buffer);
    end;
    t.CloseFunc := @FileNOPProc;
    if t.Mode = fmOutput then
    begin
      if @t = @ErrOutput then
        Std := STD_ERROR_HANDLE
      else
        Std := STD_OUTPUT_HANDLE;
    end
    else
      Std := STD_INPUT_HANDLE;
    t.Handle := GetStdHandle(Std);
  end
  else
  begin
    t.CloseFunc := @TextClose;
    Temp := CreateFile(t.Name, OpenMode, FILE_SHARE_READ, nil, Flags, FILE_ATTRIBUTE_NORMAL, 0);
    if Temp = THandle(-1) then
    begin
      t.Mode := fmClosed;
      Result := GetLastError;
      Exit;
    end;
    t.Handle := Temp;
    if t.Mode = fmInOut then      // Append mode
    begin
      t.Mode := fmOutput;

      if (t.flags and tfCRLF) <> 0 then  // DOS mode, EOF significant
      begin  // scan for EOF char in last 128 byte sector.
        Temp := GetFileSize(t.Handle, 0);
        if Temp = -1 then
        begin
          Result := TextOpenCleanup(t);
          Exit;
        end;

        Dec(Temp, 128);
        if Temp < 0 then Temp := 0;

        if (Longint(SetFilePointer(t.Handle, Temp, nil, FILE_BEGIN)) = -1) or
           (ReadFile(t.Handle, t.Buffer, 128, BytesRead, nil) = 0) then
        begin
          Result := TextOpenCleanup(t);
          Exit;
        end;

        for I := 0 to BytesRead - 1 do
        begin
          if t.Buffer[I] = AnsiChar(cEOF) then
          begin  // truncate the file here
            if (Longint(SetFilePointer(t.Handle, I - BytesRead, nil, FILE_END)) = -1) or
              (not SetEndOfFile(t.Handle)) then
            begin
              Result := TextOpenCleanup(t);
              Exit;
            end;
            Break;
          end;
        end;
      end;
    end;
  end;
  if t.Mode <> fmInput then
  begin
    case GetFileType(t.Handle) of
      0: begin  // bad file type
           TextOpenCleanup(t);
           Result := 105;
           Exit;
         end;
      2: t.FlushFunc := @TextOut;
    end;
  end;
end;
{$ELSE}
asm
// -> EAX Pointer to text record

        PUSH    ESI

        MOV     ESI,EAX

        XOR     EAX,EAX
        MOV     [ESI].TTextRec.BufPos,EAX
        MOV     [ESI].TTextRec.BufEnd,EAX
        MOV     AX,[ESI].TTextRec.Mode

        SUB     EAX,fmInput
        JE      @@calledByReset

        DEC     EAX
        JE      @@calledByRewrite

        DEC     EAX
        JE      @@calledByAppend

        JMP     @@exit

@@calledByReset:

        MOV     EAX,GENERIC_READ      // open for read
        MOV     EDX,FILE_SHARE_READ
        MOV     ECX,OPEN_EXISTING

        MOV     [ESI].TTextRec.InOutFunc,offset TextIn

        JMP     @@common

@@calledByRewrite:

        MOV     EAX,GENERIC_WRITE     // open for write
        MOV     EDX,FILE_SHARE_READ
        MOV     ECX,CREATE_ALWAYS
        JMP     @@commonOut

@@calledByAppend:

        MOV     EAX,GENERIC_READ OR GENERIC_WRITE // open for read/write
        MOV     EDX,FILE_SHARE_READ
        MOV     ECX,OPEN_EXISTING

@@commonOut:

        MOV     [ESI].TTextRec.InOutFunc,offset TextOut

@@common:

        MOV     [ESI].TTextRec.CloseFunc,offset TextClose
        MOV     [ESI].TTextRec.FlushFunc,offset FileNOPProc
        CMP     word ptr [ESI].TTextRec.Name,0
        JE      @@isCon

//  CreateFile(t.Name, EAX, EDX, Nil, ECX, FILE_ATTRIBUTE_NORMAL, 0);

        PUSH    0
        PUSH    FILE_ATTRIBUTE_NORMAL
        PUSH    ECX
        PUSH    0
        PUSH    EDX
        PUSH    EAX
        LEA     EAX,[ESI].TTextRec.Name
        PUSH    EAX
        CALL    CreateFile

        CMP     EAX,-1
        JZ      @@error

        MOV     [ESI].TTextRec.Handle,EAX
        CMP     [ESI].TTextRec.Mode,fmInOut
        JNE     @@success

        DEC     [ESI].TTextRec.Mode     // fmInOut -> fmOutput

{;  ???  we really have to look for the first eof byte in the
; ???  last record and truncate the file there.
; Not very nice and clean...
;
; lastRecPos = Max( GetFileSize(...) - 128, 0);
}
        PUSH    0
        PUSH    [ESI].TTextRec.Handle
        CALL    GetFileSize

        INC     EAX
        JZ      @@error
        SUB     EAX,129
        JNC     @@3
        XOR     EAX,EAX
@@3:
//  lseek(f.Handle, SEEK_SET, lastRecPos);

        PUSH    FILE_BEGIN
        PUSH    0
        PUSH    EAX
        PUSH    [ESI].TTextRec.Handle
        CALL    SetFilePointer

        INC     EAX
        JE      @@error

//  bytesRead = read(f.Handle, f.Buffer, 128);

        PUSH    0
        MOV     EDX,ESP
  PUSH    0
        PUSH    EDX
        PUSH    128
        LEA     EDX,[ESI].TTextRec.Buffer
        PUSH    EDX
        PUSH    [ESI].TTextRec.Handle
        CALL    ReadFile
        POP     EDX
        DEC     EAX
        JNZ     @@error

//  for (i = 0; i < bytesRead; i++)

        XOR     EAX,EAX
@@loop:
        CMP     EAX,EDX
        JAE     @@success

//    if  (f.Buffer[i] == eof)

        CMP     byte ptr [ESI].TTextRec.Buffer[EAX],eof
        JE      @@truncate
        INC     EAX
        JMP     @@loop

@@truncate:

//  lseek( f.Handle, SEEK_END, i - bytesRead );

        PUSH    FILE_END
        PUSH    0
        SUB     EAX,EDX
        PUSH    EAX
        PUSH    [ESI].TTextRec.Handle
        CALL    SetFilePointer
        INC     EAX
        JE      @@error

//  SetEndOfFile( f.Handle );

        PUSH    [ESI].TTextRec.Handle
        CALL    SetEndOfFile
        DEC     EAX
        JNE     @@error

        JMP     @@success

@@isCon:
        LEA     EAX,[ESI].TTextRec.Buffer
        MOV     [ESI].TTextRec.BufSize, TYPE TTextRec.Buffer
        MOV     [ESI].TTextRec.CloseFunc,offset FileNOPProc
        MOV     [ESI].TTextRec.BufPtr,EAX
        CMP     [ESI].TTextRec.Mode,fmOutput
        JE      @@output
        PUSH    STD_INPUT_HANDLE
        JMP     @@1
@@output:
        CMP     ESI,offset ErrOutput
        JNE     @@stdout
        PUSH    STD_ERROR_HANDLE
        JMP     @@1
@@stdout:
        PUSH    STD_OUTPUT_HANDLE
@@1:
        CALL    GetStdHandle
        CMP     EAX,-1
        JE      @@error
        MOV     [ESI].TTextRec.Handle,EAX

@@success:
  CMP     [ESI].TTextRec.Mode,fmInput
  JE      @@2
  PUSH    [ESI].TTextRec.Handle
  CALL    GetFileType
  TEST    EAX,EAX
  JE      @@badFileType
  CMP     EAX,2
  JNE     @@2
  MOV     [ESI].TTextRec.FlushFunc,offset TextOut
@@2:
  XOR     EAX,EAX
@@exit:
  POP     ESI
  RET

@@badFileType:
  PUSH    [ESI].TTextRec.Handle
  CALL    CloseHandle
  MOV     [ESI].TTextRec.Mode,fmClosed
  MOV     EAX,105
  JMP     @@exit

@@error:
  MOV     [ESI].TTextRec.Mode,fmClosed
        CALL    GetLastError
        JMP     @@exit
end;
{$IFEND CPU386}
{$ENDIF MSWINDOWS}


{$IFDEF POSIX}
var
  Flags: Integer;
  Temp, I: Integer;
  BytesRead: Integer;
  us: UTF8String;
begin
  Result := 0;
  t.BufPos := 0;
  t.BufEnd := 0;
  case t.Mode of
    fmInput: // called by Reset
      begin
        Flags := O_RDONLY;
        t.InOutFunc := @TextIn;
      end;
    fmOutput: // called by Rewrite
      begin
        Flags := O_CREAT or O_TRUNC or O_WRONLY;
        t.InOutFunc := @TextOut;
      end;
    fmInOut:  // called by Append
      begin
        Flags := O_APPEND or O_RDWR;
        t.InOutFunc := @TextOut;
      end;
  else
    Exit;
    Flags := 0;
  end;

  t.FlushFunc := @FileNOPProc;

  if t.Name[0] = #0 then  // stdin or stdout
  begin
    if t.BufPtr = nil then  // don't overwrite bufptr provided by SetTextBuf
    begin
      t.BufPtr := @t.Buffer;
      t.BufSize := sizeof(t.Buffer);
    end;
    t.CloseFunc := @FileNOPProc;
    if t.Mode = fmOutput then
    begin
      if @t = @ErrOutput then
        t.Handle := STDERR_FILENO
      else
        t.Handle := STDOUT_FILENO;
      t.FlushFunc := @TextOut;
    end
    else
      t.Handle := STDIN_FILENO;
  end
  else
  begin
    t.CloseFunc := @TextClose;

    us := UTF8String(t.Name);
    Temp := open(PAnsiChar(us), Flags, FileAccessRights);
    if Temp = -1 then
    begin
      t.Mode := fmClosed;
      Result := GetLastError;
      Exit;
    end;

    t.Handle := Temp;

    if t.Mode = fmInOut then      // Append mode
    begin
      t.Mode := fmOutput;

      if (t.flags and tfCRLF) <> 0 then  // DOS mode, EOF significant
      begin  // scan for EOF char in last 128 byte sector.
        Temp := lseek(t.Handle, 0, SEEK_END);
        if Temp = -1 then
        begin
          Result := TextOpenCleanup(t);
          Exit;
        end;

        Dec(Temp, 128);
        if Temp < 0 then Temp := 0;

        if lseek(t.Handle, Temp, SEEK_SET) = -1 then
        begin
          Result := TextOpenCleanup(t);
          Exit;
        end;

        BytesRead := __read(t.Handle, t.BufPtr, 128);
        if BytesRead = -1 then
        begin
          Result := TextOpenCleanup(t);
          Exit;
        end;

        for I := 0 to BytesRead - 1 do
        begin
          if t.Buffer[I] = AnsiChar(cEOF) then
          begin  // truncate the file here
            if ftruncate(t.Handle, lseek(t.Handle, I - BytesRead, SEEK_END)) = -1 then
            begin
              Result := TextOpenCleanup(t);
              Exit;
            end;
            Break;
          end;
        end;
      end;
    end;
  end;
end;
{$ENDIF POSIX}

const
  fNameLen = 260;

function _Assign(var t: TTextRec; const s: PChar): Integer;
var
  Len: Integer;
begin
  FillChar(t, SizeOf(TFileRec), 0);
  t.BufPtr := @t.Buffer;
  t.Mode := fmClosed;
  t.Flags := tfCRLF * Byte(DefaultTextLineBreakStyle);
  t.BufSize := SizeOf(t.Buffer);
  t.OpenFunc := @TextOpen;
  Len := _PWCharLen(s);
  MoveChars(s^, t.Name, Len);
  t.Name[Len] := #0;
  Result := 0;
end;

function InternalFlush(var t: TTextRec; Func: TTextIOFunc): Integer;
begin
  case t.Mode of
    fmOutput,
    fmInOut  : Result := Func(t);
    fmInput  : Result := 0;
  else
    if (@t = @Output) or (@t = @ErrOutput) then
      Result := 0
    else
      Result := 103;
  end;
  if Result <> 0 then SetInOutRes(Result);
end;

function Flush(var t: Text): Integer;
begin
  Result := InternalFlush(TTextRec(t), TTextRec(t).InOutFunc);
end;

function _Flush(var t: TTextRec): Integer;
begin
  Result := InternalFlush(t, t.FlushFunc);
end;

type
{$IFDEF MSWINDOWS}
{$IF defined(CPU386)}
  TIOProc = function (hFile: Integer; Buffer: Pointer; nNumberOfBytesToWrite: Cardinal;
  var lpNumberOfBytesWritten: Cardinal; lpOverlapped: Pointer): Integer; stdcall;
{$ELSE}
  TIOProc = function (hFile: THandle; Buffer: Pointer; nNumberOfBytesToWrite: Cardinal;
  var lpNumberOfBytesWritten: Cardinal; lpOverlapped: Pointer): Integer; stdcall;
{$IFEND}
{$if defined(CPU386)}
function ReadFileX(hFile: Integer; Buffer: Pointer; nNumberOfBytesToRead: Cardinal;
  var lpNumberOfBytesRead: Cardinal; lpOverlapped: Pointer): Integer; stdcall;
  external kernel name 'ReadFile';
function WriteFileX(hFile: Integer; Buffer: Pointer; nNumberOfBytesToWrite: Cardinal;
  var lpNumberOfBytesWritten: Cardinal; lpOverlapped: Pointer): Integer; stdcall;
  external kernel name 'WriteFile';
{$ELSE}
function ReadFileX(hFile: THandle; Buffer: Pointer; nNumberOfBytesToRead: Cardinal;
  var lpNumberOfBytesRead: Cardinal; lpOverlapped: Pointer): Integer; stdcall;
  external kernel name 'ReadFile';
function WriteFileX(hFile: THandle; Buffer: Pointer; nNumberOfBytesToWrite: Cardinal;
  var lpNumberOfBytesWritten: Cardinal; lpOverlapped: Pointer): Integer; stdcall;
  external kernel name 'WriteFile';
{$IFEND}
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
{$IF defined(CPU386)}
  TIOProc = function (Handle: Integer; Buffer: Pointer; Count: size_t): ssize_t; cdecl;
{$ELSE}
  TIOProc = function (Handle: THandle; Buffer: Pointer; Count: size_t): ssize_t; cdecl;
{$IFEND}
{$ENDIF POSIX}

function BlockIO(var f: TFileRec; buffer: Pointer; recCnt: Cardinal; var recsDone: Longint;
  ModeMask: Integer; IOProc: TIOProc; ErrorNo: Integer): Cardinal;
// Note:  RecsDone ptr can be nil!
begin
  if (f.Mode and ModeMask) = ModeMask then  // fmOutput or fmInOut / fmInput or fmInOut
  begin
{$IFDEF POSIX}
    Result := IOProc(f.Handle, buffer, recCnt * f.RecSize);
    if Integer(Result) = -1 then
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
    if IOProc(f.Handle, buffer, recCnt * f.RecSize, Result, nil) = 0 then
{$ENDIF MSWINDOWS}
    begin
      SetInOutRes(GetLastError);
      Result := 0;
    end
    else
    begin
      Result := Result div f.RecSize;
      if @RecsDone <> nil then
        RecsDone := Result
      else if Result <> recCnt then
      begin
        SetInOutRes(ErrorNo);
        Result := 0;
      end
    end;
  end
  else
  begin
    SetInOutRes(103);  // file not open
    Result := 0;
  end;
end;

function _BlockRead(var f: TFileRec; buffer: Pointer; recCnt: Longint; var recsRead: Longint): Longint;
begin
  Result := BlockIO(f, buffer, recCnt, recsRead, fmInput,
    {$IFDEF MSWINDOWS} ReadFileX, {$ENDIF}
    {$IFDEF POSIX} __read, {$ENDIF}
    100);
end;

function _BlockWrite(var f: TFileRec; buffer: Pointer; recCnt: Longint; var recsWritten: Longint): Longint;
begin
  Result := BlockIO(f, buffer, recCnt, recsWritten, fmOutput,
  {$IFDEF MSWINDOWS} WriteFileX, {$ENDIF}
  {$IFDEF POSIX} __write, {$ENDIF}
  101);
end;

function _Close(var t: TTextRec): Integer;
begin
  Result := 0;
  if (t.Mode >= fmInput) and (t.Mode <= fmInOut) then
  begin
    if (t.Mode and fmOutput) = fmOutput then  // fmOutput or fmInOut
      Result := TTextIOFunc(t.InOutFunc)(t);
    if Result = 0 then
      Result := TTextIOFunc(t.CloseFunc)(t);
    if Result <> 0 then
      SetInOutRes(Result);
  end
  else
  if @t <> @Input then
    SetInOutRes(103);
end;

function _EofFile(var f: TFileRec): Boolean;
begin
  Result := _FilePos(f) >= _FileSize(f);
end;




function _EofText(var t: TTextRec): Boolean;
{$IF not defined(CPU386)}
var
  c: Word;
begin
  if (t.Mode = fmInput) and (t.BufPos < t.BufEnd) then
  begin
    Result := ((t.Flags and tfCRLF) <> 0) and
              (Byte(t.BufPtr[t.BufPos]) = cEof);
  end
  else
  begin
    c := _ReadCharEx(t);
    if (c and $ff00) shr 8 = cEof then Exit(True);
    Dec(t.BufPos);
    Result := False;
  end;
end;
{$ELSE}
asm
// -> EAX Pointer to text record
// <- AL  Boolean result
        CMP     [EAX].TTextRec.Mode,fmInput
        JNE     @@readChar
        MOV     EDX,[EAX].TTextRec.BufPos
        CMP     EDX,[EAX].TTextRec.BufEnd
        JAE     @@readChar
        ADD     EDX,[EAX].TTextRec.BufPtr
        TEST    [EAX].TTextRec.Flags,tfCRLF
        JZ      @@FalseExit
        MOV     CL,[EDX]
        CMP     CL,cEof
        JNZ     @@FalseExit

@@eof:
        MOV     AL,1
        JMP     @@exit

@@readChar:
        PUSH    EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    _ReadChar
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        POP     EDX
        CMP     AH,cEof
        JE      @@eof
        DEC     [EDX].TTextRec.BufPos
@@FalseExit:
        XOR     EAX,EAX
@@exit:
end;
{$IFEND}




function _Eoln(var t: TTextRec): Boolean;
{$IF not defined(CPU386)}
var
  c: Word;
begin
  if (t.Mode = fmInput) and (t.BufPos < t.BufEnd) then
    c := Byte(t.BufPtr[t.BufPos])
  else
  begin
    c := _ReadCharEx(t);
    if (c and $ff00) shr 8 = cEof then Exit(True);
    Dec(t.BufPos);
    c := c and $ff;
  end;
  if (t.Flags and tfCRLF) <> 0 then
    Result := (c = cCR) or (c = cEOF)
  else
    Result := (c = cLF) or (c = cEOF);
end;
{$ELSE}
asm
// -> EAX Pointer to text record
// <- AL  Boolean result

        CMP     [EAX].TTextRec.Mode,fmInput
        JNE     @@readChar
        MOV     EDX,[EAX].TTextRec.BufPos
        CMP     EDX,[EAX].TTextRec.BufEnd
        JAE     @@readChar
        ADD     EDX,[EAX].TTextRec.BufPtr
        TEST    [EAX].TTextRec.Flags,tfCRLF
        MOV     AL,0
        MOV     CL,[EDX]
        JZ      @@testLF
        CMP     CL,cCR
        JE      @@eol
        CMP     CL,cEOF
        JE      @@eof
        JMP     @@exit

@@testLF:
        CMP     CL,cLF
        JE      @@eol
        CMP     CL,cEOF
        JE      @@eof
        JMP     @@exit

@@readChar:
        PUSH    EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    _ReadChar
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        POP     EDX
        CMP     AH,cEOF
        JE      @@eof
        DEC     [EDX].TTextRec.BufPos
        XOR     ECX,ECX
        XCHG    ECX,EAX
        TEST    [EDX].TTextRec.Mode,tfCRLF
        JE      @@testLF
        CMP     CL,cCR
        JE      @@eol
        JMP     @@exit

@@eol:
@@eof:
        MOV     AL,1
@@exit:
end;
{$IFEND}

procedure _Erase(var f: TFileRec);
begin
  if (f.Mode < fmClosed) or (f.Mode > fmInOut) then
    SetInOutRes(102)  // file not assigned
  else begin
{$IFDEF MSWINDOWS}
    if not DeleteFile(f.Name) then
      SetInOutRes(GetLastError);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
    if remove(PAnsiChar(UTF8Encode(f.Name))) < 0 then
       SetInOutRes(GetLastError);
{$ENDIF POSIX}
  end;
end;

{$IF defined(MSWINDOWS) and defined(CPU386)}
// Floating-point divide reverse routine
// ST(1) = ST(0) / ST(1), pop ST


procedure _FSafeDivideR;
asm
  FXCH
  JMP _FSafeDivide
end;

// Floating-point divide routine
// ST(1) = ST(1) / ST(0), pop ST



procedure _FSafeDivide;
type
  Z = packed record  // helper type to make parameter references more readable
    Dividend: Extended;   // (TBYTE PTR [ESP])
    Pad: Word;
    Divisor: Extended;    // (TBYTE PTR [ESP+12])
  end;
asm
        CMP       TestFDIV,0        //Check FDIV indicator
        JLE       @@FDivideChecked  //Jump if flawed or don't know
        FDIV                        //Known to be ok, so just do FDIV
        RET

// FDIV constants
@@FDIVRiscTable: DB 0,1,0,0,4,0,0,7,0,0,10,0,0,13,0,0;

@@FDIVScale1:    DD $3F700000             // 0.9375
@@FDIVScale2:    DD $3F880000             // 1.0625
@@FDIV1SHL63:    DD $5F000000             // 1 SHL 63

@@TestDividend:  DD $C0000000,$4150017E   // 4195835.0
@@TestDivisor:   DD $80000000,$4147FFFF   // 3145727.0
@@TestOne:       DD $00000000,$3FF00000   // 1.0

// Flawed FDIV detection
@@FDivideDetect:
        MOV     TestFDIV,1                //Indicate correct FDIV
        PUSH    EAX
        SUB     ESP,12
        FSTP    TBYTE PTR [ESP]           //Save off ST
        FLD     QWORD PTR @@TestDividend  //Ok if x - (x / y) * y < 1.0
        FDIV    QWORD PTR @@TestDivisor
        FMUL    QWORD PTR @@TestDivisor
        FSUBR   QWORD PTR @@TestDividend
        FCOMP   QWORD PTR @@TestOne
        FSTSW   AX
        SHR     EAX,7
        AND     EAX,002H          //Zero if FDIV is flawed
        DEC     EAX
        MOV     TestFDIV,AL       //1 means Ok, -1 means flawed
        FLD     TBYTE PTR [ESP]   //Restore ST
        ADD     ESP,12
        POP     EAX
        JMP     _FSafeDivide

@@FDivideChecked:
        JE      @@FDivideDetect   //Do detection if TestFDIV = 0
@@1:
        PUSH    EAX
        SUB     ESP,24
        FSTP    [ESP].Z.Divisor     //Store Divisor and Dividend
        FSTP    [ESP].Z.Dividend
        FLD     [ESP].Z.Dividend
        FLD     [ESP].Z.Divisor
@@2:
        MOV     EAX,DWORD PTR [ESP+4].Z.Divisor   //Is Divisor a denormal?
        ADD     EAX,EAX
        JNC     @@20            //Yes, @@20
        XOR     EAX,0E000000H   //If these three bits are not all
        TEST    EAX,0E000000H   //ones, FDIV will work
        JZ      @@10            //Jump if all ones
@@3:
        FDIV                    //Do FDIV and exit
        ADD     ESP,24
        POP     EAX
        RET
@@10:
        SHR     EAX,28      //If the four bits following the MSB
                            //of the mantissa have a decimal
                            //of 1, 4, 7, 10, or 13, FDIV may
        CMP     byte ptr @@FDIVRiscTable[EAX],0 //not work correctly
        JZ      @@3     //Do FDIV if not 1, 4, 7, 10, or 13
        MOV     EAX,DWORD PTR [ESP+8].Z.Divisor //Get Divisor exponent
        AND     EAX,7FFFH
        JZ      @@3     //Ok to FDIV if denormal
        CMP     EAX,7FFFH
        JE      @@3     //Ok to FDIV if NAN or INF
        MOV     EAX,DWORD PTR [ESP+8].Z.Dividend //Get Dividend exponent
        AND     EAX,7FFFH
        CMP     EAX,1     //Small number?
        JE      @@11      //Yes, @@11
        FMUL    DWORD PTR @@FDIVScale1  //Scale by 15/16
        FXCH
        FMUL    DWORD PTR @@FDIVScale1
        FXCH
        JMP     @@3     //FDIV is now safe
@@11:
        FMUL    DWORD PTR @@FDIVScale2    //Scale by 17/16
        FXCH
        FMUL    DWORD PTR @@FDIVScale2
        FXCH
        JMP     @@3     //FDIV is now safe

@@20:
        MOV     EAX,DWORD PTR [ESP].Z.Divisor     //Is entire Divisor zero?
        OR      EAX,DWORD PTR [ESP+4].Z.Divisor
        JZ      @@3               //Yes, ok to FDIV
        MOV     EAX,DWORD PTR [ESP+8].Z.Divisor   //Get Divisor exponent
        AND     EAX,7FFFH         //Non-zero exponent is invalid
        JNZ     @@3               //Ok to FDIV if invalid
        MOV     EAX,DWORD PTR [ESP+8].Z.Dividend  //Get Dividend exponent
        AND     EAX,7FFFH         //Denormal?
        JZ      @@21              //Yes, @@21
        CMP     EAX,7FFFH         //NAN or INF?
        JE      @@3               //Yes, ok to FDIV
        MOV     EAX,DWORD PTR [ESP+4].Z.Dividend  //If MSB of mantissa is zero,
        ADD     EAX,EAX           //the number is invalid
        JNC     @@3               //Ok to FDIV if invalid
        JMP     @@22
@@21:
        MOV     EAX,DWORD PTR [ESP+4].Z.Dividend  //If MSB of mantissa is zero,
        ADD     EAX,EAX                           //the number is invalid
        JC      @@3                               //Ok to FDIV if invalid
@@22:
        FXCH                  //Scale stored Divisor image by
        FSTP    ST(0)         //1 SHL 63 and restart
        FLD     ST(0)
        FMUL    DWORD PTR @@FDIV1SHL63
        FSTP    [ESP].Z.Divisor
        FLD     [ESP].Z.Dividend
        FXCH
        JMP     @@2
end;
{$IFEND MSWINDOWS and CPU386}

function _FilePos(var f: TFileRec): Longint;
begin
  if (f.Mode > fmClosed) and (f.Mode <= fmInOut) then
  begin
{$IFDEF MSWINDOWS}
    Result := Longint(SetFilePointer(f.Handle, 0, nil, FILE_CURRENT));
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}

    Result := lseek(f.Handle, 0, SEEK_CUR);
{$ENDIF POSIX}
    if Result = -1 then
      InOutError
    else
      Result := Cardinal(Result) div f.RecSize;
  end
  else
  begin
    SetInOutRes(103);
    Result := -1;
  end;
end;

function _FileSize(var f: TFileRec): Longint;
{$IFDEF MSWINDOWS}
begin
  Result := -1;
  if (f.Mode > fmClosed) and (f.Mode <= fmInOut) then
  begin
    Result := GetFileSize(f.Handle, 0);
    if Result = -1 then
      InOutError
    else
      Result := Cardinal(Result) div f.RecSize;
  end
  else
    SetInOutRes(103);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  stat: _stat;
begin
  Result := -1;
  if (f.Mode > fmClosed) and (f.Mode <= fmInOut) then
  begin
//    if _fxstat(STAT_VER_LINUX, f.Handle, stat) <> 0 then
    if fstat(f.Handle, stat) <> 0 then
      InOutError
    else
      Result := stat.st_size div f.RecSize;
  end
  else
    SetInOutRes(103);
{$ENDIF POSIX}
end;





(* ***** BEGIN LICENSE BLOCK *****
 *
 * The assembly procedure FillChar is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): John O'Harrow
 *
 * ***** END LICENSE BLOCK ***** *)
procedure _FillChar(var Dest; count: Integer; Value: AnsiChar);
{$IF defined(PUREPASCAL)}
var
  I: Integer;
  P: PByte;
begin
  P := PByte(@Dest);
  for I := count-1 downto 0 do
    P[I] := Byte(Value);
end;
{$ELSE !PUREPASCAL}
asm                                  // Size = 153 Bytes
        CMP   EDX, 32
        MOV   CH, CL                 // Copy Value into both Bytes of CX
        JL    @@Small
        MOV   [EAX  ], CX            // Fill First 8 Bytes
        MOV   [EAX+2], CX
        MOV   [EAX+4], CX
        MOV   [EAX+6], CX
        SUB   EDX, 16
        FLD   QWORD PTR [EAX]
        FST   QWORD PTR [EAX+EDX]    // Fill Last 16 Bytes
        FST   QWORD PTR [EAX+EDX+8]
        MOV   ECX, EAX
        AND   ECX, 7                 // 8-Byte Align Writes
        SUB   ECX, 8
        SUB   EAX, ECX
        ADD   EDX, ECX
        ADD   EAX, EDX
        NEG   EDX
@@Loop:
        FST   QWORD PTR [EAX+EDX]    // Fill 16 Bytes per Loop
        FST   QWORD PTR [EAX+EDX+8]
        ADD   EDX, 16
        JL    @@Loop
        FFREE ST(0)
        FINCSTP
        RET
        NOP
        NOP
        NOP
@@Small:
        TEST  EDX, EDX
        JLE   @@Done
        MOV   [EAX+EDX-1], CL        // Fill Last Byte
        AND   EDX, -2                // No. of Words to Fill
        NEG   EDX
{$IFDEF PIC}
        PUSH  EAX
        PUSH  EBX
        PUSH  ECX
        CALL  GetGOT
        ADD   EAX, offset @@SmallFill + 60
        LEA   EDX, [EAX + EDX * 2]
        POP   ECX
        POP   EBX
        POP   EAX
{$ELSE !PIC}
        LEA   EDX, [@@SmallFill + 60 + EDX * 2]
{$ENDIF !PIC}
        JMP   EDX
{$IFNDEF PIC}
        NOP                          // Align Jump Destinations
        NOP
{$ENDIF !PIC}
@@SmallFill:
        MOV   [EAX+28], CX
        MOV   [EAX+26], CX
        MOV   [EAX+24], CX
        MOV   [EAX+22], CX
        MOV   [EAX+20], CX
        MOV   [EAX+18], CX
        MOV   [EAX+16], CX
        MOV   [EAX+14], CX
        MOV   [EAX+12], CX
        MOV   [EAX+10], CX
        MOV   [EAX+ 8], CX
        MOV   [EAX+ 6], CX
        MOV   [EAX+ 4], CX
        MOV   [EAX+ 2], CX
        MOV   [EAX   ], CX
        RET                          // DO NOT REMOVE - This is for Alignment
@@Done:
end;
{$IFEND !PUREPASCAL and !PIC}

procedure       Mark;
begin
  Error(reInvalidPtr);
end;


function _ReadRec(var f: TFileRec; Buffer: Pointer): Integer;
{$IFDEF MSWINDOWS}
{$IF not defined(CPU386)}
begin
  if (f.Mode = fmInput) or (f.Mode = fmInOut) then
  begin
    if ReadFile(f.Handle, Buffer^, f.RecSize, Cardinal(Result), nil) = 0 then
      SetInOutRes(GetLastError)
    else if Cardinal(Result) <> f.RecSize then
      SetInOutRes(100);
  end
  else
  begin
    SetInOutRes(103);  // file not open for input
    Result := 0;
  end;
end;
{$ELSE}
asm
// -> EAX Pointer to file variable
//    EDX Pointer to buffer

        PUSH    EBX
        XOR     ECX,ECX
        MOV     EBX,EAX
        MOV     CX,[EAX].TFileRec.Mode   // File must be open
        SUB     ECX,fmInput
        JE      @@skip
        SUB     ECX,fmInOut-fmInput
        JNE     @@fileNotOpen
@@skip:

//  ReadFile(f.Handle, buffer, f.RecSize, @result, Nil);

        PUSH    0     // space for OS result
        MOV     EAX,ESP

        PUSH    0     // pass lpOverlapped
        PUSH    EAX     // pass @result

        PUSH    [EBX].TFileRec.RecSize    // pass nNumberOfBytesToRead

        PUSH    EDX     // pass lpBuffer
        PUSH    [EBX].TFileRec.Handle   // pass hFile
        CALL    ReadFile
        POP     EDX     // pop result
        DEC     EAX     // check EAX = TRUE
        JNZ     @@error

        CMP     EDX,[EBX].TFileRec.RecSize  // result = f.RecSize ?
        JE      @@exit

@@readError:
        MOV EAX,100
        JMP @@errExit

@@fileNotOpen:
        MOV EAX,103
        JMP @@errExit

@@error:
        CALL  GetLastError
@@errExit:
        CALL  SetInOutRes
@@exit:
        POP EBX
end;
{$IFEND}
{$ENDIF MSWINDOWS}


{$IFDEF POSIX}
begin
  if (f.Mode and fmInput) = fmInput then  // fmInput or fmInOut
  begin
    Result := __read(f.Handle, Buffer, f.RecSize);
    if Result = -1 then
      InOutError
    else if Cardinal(Result) <> f.RecSize then
      SetInOutRes(100);
  end
  else
  begin
    SetInOutRes(103);  // file not open for input
    Result := 0;
  end;
end;
{$ENDIF POSIX}

// If the file is Input std variable, try to open it
// Otherwise, runtime error.
function TryOpenForInput(var t: TTextRec): Boolean;
begin
  if @t = @Input then
  begin
    t.Flags := tfCRLF * Byte(DefaultTextLineBreakStyle);
    _ResetText(t);
  end;

  Result := t.Mode = fmInput;
  if not Result then
    SetInOutRes(104);
end;


{$IF not defined(CPU386)}
// <- Lower 8 bits:  Character read. (may be a pseudo cEOF in DOS mode)
// <- Upper 8 bits: cEOF = End of File, else 0
//    For eof, #$1A is returned in both lower and upper 8 bits.
//    For errors, InOutRes is set and #$1A is returned.
function _ReadCharEx(var t: TTextRec): Word;
const
  resEof = (cEof shl 8) or cEof;
var
  Res: Integer;
  c: Byte;
begin
  if t.Mode <> fmInput then
    if TryOpenForInput(t) then
      Exit(resEof);

  if t.BufPos >= t.BufEnd then
  begin
    Res := TTextIOFunc(t.InOutFunc)(t);
    if Res <> 0 then
    begin
      SetInOutRes(Res);
      Exit(resEof);
    end;
    if t.BufPos >= t.BufEnd then
    begin
      //  We didn't get characters. Must be eof then.
      if (t.Flags and tfCRLF) <> 0 then
      begin
        //  In DOS CRLF compatibility mode, synthesize an EOF char
        //  Store one eof in the buffer and increment BufEnd
        t.BufPtr[t.BufPos] := AnsiChar(cEof);
        inc(t.BufEnd);
      end;
      Exit(resEof);
    end;
  end;

  c := Ord(t.BufPtr[t.BufPos]);
  // Check for EOF char in DOS mode
  if ((t.Flags and tfCRLF) <> 0) and (c = cEof) then Exit(resEof);
  Inc(t.BufPos);
  Result := c;
end;
{$IFEND}




{$IF not defined(CPU386)}
function _ReadChar(var t: TTextRec): AnsiChar;
begin
  Result := AnsiChar(_ReadCharEx(t));
end;
{$ELSE}
function _ReadChar(var t: TTextRec): AnsiChar;
asm //StackAlignSafe
// -> EAX Pointer to text record
// <- AL  Character read. (may be a pseudo cEOF in DOS mode)
// <- AH  cEOF = End of File, else 0
//    For eof, #$1A is returned in AL and in AH.
//    For errors, InOutRes is set and #$1A is returned.

{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        CMP     [EAX].TTextRec.Mode, fmInput
        JE      @@checkBuf
        PUSH    EAX
        CALL    TryOpenForInput
        TEST    AL,AL
        POP     EAX
        JZ      @@eofexit

@@checkBuf:
        MOV     EDX,[EAX].TTextRec.BufPos
        CMP     EDX,[EAX].TTextRec.BufEnd
        JAE     @@fillBuf
@@cont:
        TEST    [EAX].TTextRec.Flags, tfCRLF
        MOV     ECX,[EAX].TTextRec.BufPtr
        MOV     CL,[ECX+EDX]
        JZ      @@cont2
        CMP     CL,cEof                       // Check for EOF char in DOS mode
        JE      @@eofexit
@@cont2:
        INC     EDX
        MOV     [EAX].TTextRec.BufPos,EDX
        XOR     EAX,EAX
        JMP     @@exit

@@fillBuf:
        PUSH    EAX
        CALL    [EAX].TTextRec.InOutFunc
        TEST    EAX,EAX
        JNE     @@error
        POP     EAX
        MOV     EDX,[EAX].TTextRec.BufPos
        CMP     EDX,[EAX].TTextRec.BufEnd
        JB      @@cont

//  We didn't get characters. Must be eof then.

@@eof:
        TEST    [EAX].TTextRec.Flags, tfCRLF
        JZ      @@eofexit
//  In DOS CRLF compatibility mode, synthesize an EOF char
//  Store one eof in the buffer and increment BufEnd
        MOV     ECX,[EAX].TTextRec.BufPtr
        MOV     byte ptr [ECX+EDX],cEof
        INC     [EAX].TTextRec.BufEnd
        JMP     @@eofexit

@@error:
        CALL    SetInOutRes
        POP     EAX
@@eofexit:
        MOV     CL,cEof
        MOV     AH,CL
@@exit:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        MOV     AL,CL
end;
{$IFEND}


function _ValLongL(const s: AnsiString; var code: Integer): Longint;
begin
  Result := _ValLong(string(s), code);
end;




function _ReadLong(var t: TTextRec): Longint;
{$IF not defined(CPU386)}
type
  TStrRec32 = packed record
    hdr: StrRec;
    data: array[0..35] of Byte;
  end;
var
  s: TStrRec32;
  c: Word;
  p: PByte;
  count: Integer;
  code: Integer;
begin
  if _SeekEof(t) then
    Result := 0
  else
  begin
    p := @s.data[0];
    for count := 1 to 32 do
    begin
      c := _ReadCharEx(t);
      if (c and $ff) <= $20 then
      begin
        if ((c and $ff00) shr 8) <> cEof then Dec(t.BufPos);
        break;
      end;
      p^ := c;
      Inc(p);
    end;
    p^ := 0;
    s.hdr.codePage := CP_ACP;
    s.hdr.elemSize := 1;
    s.hdr.refCnt := -1;
    s.hdr.length := p - PByte(@s.data[0]);
    Result := _ValLongL(PAnsiString(@s.data)^, code);
    if code <> 0 then
      SetInOutRes(106);
  end;
end;
{$ELSE}
asm
// -> EAX Pointer to text record
// <- EAX Result

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        SUB     ESP,36       // var numbuf: String[32];
        PUSH     0           // String Length
        PUSH     -1          // Refcount (-1 = string constant)
        PUSH     $00010000   // elemSize = 1, codePage = CP_ACP

        MOV     ESI,EAX
        CALL    _SeekEof
        DEC     AL
        JZ      @@eof

        LEA     EDI,[ESP+skew]     // EDI -> numBuf[0]
        MOV     BL,32
@@loop:
        MOV     EAX,ESI
        CALL    _ReadChar
        CMP     AL,' '
        JBE     @@endNum
        STOSB
        DEC     BL
        JNZ     @@loop
@@convert:
        MOV     byte ptr [EDI],0
        LEA     EAX,[ESP+skew]     // EAX -> numBuf
        MOV      ECX,EDI
        SUB      ECX,EAX
        MOV      [EAX-Skew].StrRec.length,ECX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EDX          // allocate code result
        MOV     EDX,ESP      // pass pointer to code
        CALL    _ValLongL    // convert
        POP     EDX          // pop code result into EDX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        TEST    EDX,EDX
        JZ      @@exit
        MOV     EAX,106
        CALL    SetInOutRes
        JMP     @@exit

@@endNum:
        CMP     AH,cEof
        JE      @@convert
        DEC     [ESI].TTextRec.BufPos
        JMP     @@convert

@@eof:
        XOR     EAX,EAX
@@exit:
        ADD     ESP,36 + 4 + 4 + 2 + 2 // length, refCnt, elemSize, codePage
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$IFEND}




{$IF not defined(CPU386)}
function ReadLineEx(var t: TTextRec; buf: Pointer; maxLen: Longint; var Count: Integer): Pointer;
begin
end;
{$ELSE}
function ReadLine(var t: TTextRec; buf: Pointer; maxLen: Longint): Pointer;
asm //StackAlignSafe
// -> EAX Pointer to text record
//    EDX Pointer to buffer
//    ECX Maximum count of chars to read
// <- ECX Actual count of chars in buffer
// <- EAX Pointer to text record

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    ECX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        MOV     ESI,ECX
        MOV     EDI,EDX

        CMP     [EAX].TTextRec.Mode,fmInput
        JE      @@start
        PUSH    EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    TryOpenForInput
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        TEST    AL,AL
        POP     EAX
        JZ      @@exit

@@start:
        MOV     EBX,EAX

        TEST    ESI,ESI
        JLE     @@exit

        MOV     EDX,[EBX].TTextRec.BufPos
        MOV     ECX,[EBX].TTextRec.BufEnd
        SUB     ECX,EDX
        ADD     EDX,[EBX].TTextRec.BufPtr

@@loop:
        DEC     ECX
        JL      @@readChar
        MOV     AL,[EDX]
        INC     EDX
@@cont:
        CMP     AL,cLF
        JE      @@lf

        CMP     AL,cCR
        JE      @@cr

        CMP     AL,cEOF
        JE      @@eof

@@store:
        STOSB
        DEC     ESI
        JG      @@loop
        JMP     @@finish

@@eof:
        TEST    [EBX].TTextRec.Flags,tfCRLF
        JZ      @@store
        JMP     @@finish

@@cr:
        MOV     AL,[EDX]
        CMP     AL,cLF
        JNE     @@loop
@@lf:
        DEC     EDX
@@finish:
        SUB     EDX,[EBX].TTextRec.BufPtr
        MOV     [EBX].TTextRec.BufPos,EDX
        JMP     @@exit

@@readChar:
        MOV     [EBX].TTextRec.BufPos,EDX
        MOV     EAX,EBX
        CALL    _ReadChar
        MOV     EDX,[EBX].TTextRec.BufPos
        MOV     ECX,[EBX].TTextRec.BufEnd
        SUB     ECX,EDX
        ADD     EDX,[EBX].TTextRec.BufPtr
        TEST    AH,AH    //eof
        JZ      @@cont

@@exit:
        MOV     EAX,EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        POP     ECX
        SUB     ECX,ESI
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$IFEND}



procedure _ReadString(var t: TTextRec; s: PShortString; maxLen: Longint);
{$IF not defined(CPU386)}
var
  Count: Integer;
begin
  ReadLineEx(t, @s^[1], maxLen, Count);
  Byte(s^[0]) := Count;
end;
{$ELSE}
asm //StackAlignSafe
// -> EAX Pointer to text record
//    EDX Pointer to string
//    ECX Maximum length of string

{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH     EDX
        INC      EDX
        CALL     ReadLine
        POP      EDX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        MOV      [EDX],CL
end;
{$IFEND}



procedure _ReadCString(var t: TTextRec; s: PAnsiChar; maxLen: Longint);
{$IF not defined(CPU386)}
var
  Count: Integer;
begin
  ReadLineEx(t, s, maxLen, Count);
  s[Count] := #0;
end;
{$ELSE}
asm //StackAlignSafe
// -> EAX Pointer to text record
//    EDX Pointer to string
//    ECX Maximum length of string

{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EDX
        CALL    ReadLine
        POP     EDX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        MOV     byte ptr [EDX+ECX],0
end;
{$IFEND}



procedure _ReadLString(var t: TTextRec; var s: AnsiString; CodePage: Word);
{$IF not defined(CPU386)}
var
  Count: Integer;
  Buf: ShortString;
  Temp: AnsiString;
begin
  _LStrClr(s);
  _ReadString(t, @Buf, 255);
  _LStrFromString(s, Buf, CodePage);
  while Byte(Buf[0]) = 255 do
  begin
    _ReadString(t, @Buf, 255);
    _LStrFromString(Temp, Buf, CodePage);
    _LStrCat(s, Temp);
    _LStrClr(Temp);
  end;
end;
{$ELSE}
asm //StackAlignSafe
        { ->    EAX     pointer to Text         }
        {       EDX     pointer to AnsiString   }
        {       ECX     destination codepage    }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        MOV     EAX,EDX
        CALL    _LStrClr

        SUB     ESP,256

        MOV     EAX,EBX
        MOV     EDX,ESP
        MOV     ECX,255
        CALL    _ReadString

        MOV     EAX,ESI
        MOV     EDX,ESP
        MOV     ECX,EDI
        CALL    _LStrFromString

        CMP     byte ptr [ESP],255
        JNE     @@exit
@@loop:

        MOV     EAX,EBX
        MOV     EDX,ESP
        MOV     ECX,255
        CALL    _ReadString

        MOV     EDX,ESP
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    0
        MOV     EAX,ESP
        MOV     ECX,EDI
        CALL    _LStrFromString

        MOV     EAX,ESI
        MOV     EDX,[ESP]
        CALL    _LStrCat

        MOV     EAX,ESP
        CALL    _LStrClr
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}

        CMP     byte ptr [ESP],255
        JE      @@loop

@@exit:
        ADD     ESP,256
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$IFEND}


procedure _ReadUString(var t: TTextRec; var s: UnicodeString);
var
  Temp: AnsiString;
begin
  // !!! FIXME
  _ReadLString(t, Temp, DefaultSystemCodePage);
  s := UnicodeString(Temp);
end;

function _ReadWChar(var t: TTextRec): WideChar;
  function IsValidMultibyteChar(const Src: PAnsiChar;
    SrcBytes: Integer): Boolean; inline;
  const
    MB_ERR_INVALID_CHARS         = 8;
  var
    Dest: WideChar;
  begin
    Result := UnicodeFromLocaleChars(DefaultSystemCodePage, MB_ERR_INVALID_CHARS,
      Src, SrcBytes, @Dest, 1) <> 0;
  end;
var
  scratch: array [0..7] of AnsiChar;
  wc: WideChar;
  i: Integer;
begin
  i := 0;
  while i < High(scratch) do
  begin
    scratch[i] := _ReadChar(t);
    Inc(i);
    scratch[i] := #0;
    if IsValidMultibyteChar(scratch, i) then
    begin
      WCharFromChar(@wc, 1, scratch, i, DefaultSystemCodePage);
      Result := wc;
      Exit;
    end;
  end;
  SetInOutRes(106);  // Invalid Input
  Result := #0;
//  Result := _ReadChar(t);
end;


procedure _ReadWCString(var t: TTextRec; s: PWideChar; maxBytes: Longint);
var
  i, maxLen: Integer;
  wc: WideChar;
begin
  if s = nil then Exit;
  i := 0;
  maxLen := maxBytes div sizeof(WideChar);
  while i < maxLen do
  begin
    wc := _ReadWChar(t);
    case Integer(wc) of
      cEOF: if _EOFText(t) then Break;
      cLF : begin
              Dec(t.BufPos);
              Break;
            end;
      cCR : if Byte(t.BufPtr[t.BufPos]) = cLF then
            begin
              Dec(t.BufPos);
              Break;
            end;
    end;
    s[i] := wc;
    Inc(i);
  end;
  s[i] := #0;
end;

procedure _ReadWString(var t: TTextRec; var s: WideString);
var
  Temp: AnsiString;
begin
  _ReadLString(t, Temp, DefaultSystemCodePage);
  s := WideString(Temp);
end;



function _ValExtL(const s: AnsiString; var code: Integer): Extended;
begin
  Val(string(s), Result, code);
end;



function _ReadExt(var t: TTextRec): Extended;
{$IF not defined(CPU386)}
type
  TStrRec64 = packed record
    hdr: StrRec;
    data: array[0..67] of Byte;
  end;
var
  s: TStrRec64;
  c: Word;
  p: PByte;
  count: Integer;
  code: Integer;
begin
  if _SeekEof(t) then
    Result := 0
  else
  begin
    p := @s.data[0];
    for count := 1 to 64 do
    begin
      c := _ReadCharEx(t);
      if (c and $ff) <= $20 then
      begin
        if ((c and $ff00) shr 8) <> cEof then Dec(t.BufPos);
        break;
      end;
      p^ := c;
      Inc(p);
    end;
    p^ := 0;
    s.hdr.codePage := CP_ACP;
    s.hdr.elemSize := 1;
    s.hdr.refCnt := -1;
    s.hdr.length := p - PByte(@s.data[0]);
    Result := _ValExtL(PAnsiString(@s.data)^, code);
    if code <> 0 then
      SetInOutRes(106);
  end;
end;
{$ELSE}
asm //StackAlignSafe
// -> EAX Pointer to text record
// <- FST(0)  Result

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        SUB     ESP,68      // var numbuf: array[0..64] of char;
        PUSH    0           // String Length
        PUSH    -1          // Refcount (-1 = string constant)
        PUSH    $00010000   // elemSize = 1, codePage = CP_ACP
        // stack should be 16-byte aligned at this point

        MOV     ESI,EAX
        CALL    _SeekEof
        DEC     AL
        JZ      @@eof

        LEA     EDI,[ESP+Skew]     // EDI -> numBuf[0]
        MOV     BL,64
@@loop:
        MOV     EAX,ESI
        CALL    _ReadChar
        CMP     AL,' '
        JBE     @@endNum
        STOSB
        DEC     BL
        JNZ     @@loop
@@convert:
        MOV     byte ptr [EDI],0
        LEA     EAX,[ESP+Skew]     // EAX -> numBuf
        MOV     ECX,EDI
        SUB     ECX,EAX
        MOV     [EAX-Skew].StrRec.length,ECX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EDX     // allocate code result
        MOV     EDX,ESP     // pass pointer to code
        CALL    _ValExtL     // convert
        POP     EDX     // pop code result into EDX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        TEST    EDX,EDX
        JZ      @@exit
        MOV     EAX,106
        CALL    SetInOutRes
        JMP     @@exit

@@endNum:
        CMP     AH,cEOF
        JE      @@convert
        DEC     [ESI].TTextRec.BufPos
        JMP     @@convert

@@eof:
        FLDZ
@@exit:
        ADD     ESP,68 + 4 + 4 + 2 + 2 // length, refcount, elemsize, codepage
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$IFEND}



procedure _ReadLn(var t: TTextRec);
{$IF not defined(CPU386)}
var
  c: Word;
begin
  while True do
  begin
    c := _ReadCharEx(t);
    if (c and $00ff) = cLF then break; // accept LF as end of line
    if (c and $ff00) shr 8 = cEOF then break;
    if (c and $00ff) = cCR then
    begin
      c := _ReadCharEx(t);
      if (c and $00ff) = cLF then break; // accept CR+LF as end of line
      if (c and $ff00) shr 8 = cEOF then break; // accept CR+EOF as end of line
      Dec(t.BufPos);
      // else CR+ anything else is not a line break.
    end;
  end;
end;
{$ELSE}
asm //StackAlignSafe
// -> EAX Pointer to text record

        PUSH    EBX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        MOV     EBX,EAX
@@loop:
        MOV     EAX,EBX
        CALL    _ReadChar

        CMP     AL,cLF            // accept LF as end of line
        JE      @@exit
        CMP     AH,cEOF
        JE      @@eof
        CMP     AL,cCR
        JNE     @@loop

        MOV     EAX,EBX
        CALL    _ReadChar

        CMP     AL,cLF            // accept CR+LF as end of line
        JE      @@exit
        CMP     AH,cEOF           // accept CR+EOF as end of line
        JE      @@eof
        DEC     [EBX].TTextRec.BufPos
        JMP     @@loop            // else CR+ anything else is not a line break.

@@exit:
@@eof:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        POP     EBX
end;
{$IFEND}

procedure _Rename(var f: TFileRec; newName: PChar);
var
  I: Integer;
  oldName: string;
{$IFDEF POSIX}
  usOldName: UTF8String;
  usNewName: UTF8String;
{$ENDIF POSIX}
begin
  if f.Mode = fmClosed then
  begin
    if newName = nil then newName := '';
    oldName := f.Name;
{$IFDEF POSIX}
    usNewName := UTF8String(newName);
    usOldName := UTF8String(oldName);
    if __rename(PAnsiChar(usOldName), PAnsiChar(usNewName)) = 0 then
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
    if MoveFile(PChar(oldName), newName) then
{$ENDIF MSWINDOWS}
    begin
      I := 0;
      while (newName[I] <> #0) and (I < High(f.Name)) do
      begin
        f.Name[I] := newName[I];
        Inc(I);
      end
    end
    else
      SetInOutRes(GetLastError);
  end
  else
    SetInOutRes(102);
end;

procedure Release;
begin
  Error(reInvalidPtr);
end;

function _CloseFile(var f: TFileRec): Integer;
begin
  f.Mode := fmClosed;
  Result := 0;
  if not InternalClose(f.Handle) then
  begin
    InOutError;
    Result := 1;
  end;
end;

function OpenFile(var f: TFileRec; recSiz: Longint; mode: Longint): Integer;
{$IFDEF POSIX}
var
   Flags: Integer;
   uName: UTF8String;
begin
  Result := 0;
  if (f.Mode >= fmClosed) and (f.Mode <= fmInOut) then
  begin
    if f.Mode <> fmClosed then // not yet closed: close it
    begin
      Result := TFileIOFunc(f.CloseFunc)(f);
      if Result <> 0 then
        SetInOutRes(Result);
    end;

    if recSiz <= 0 then
      SetInOutRes(106);

    f.RecSize := recSiz;
    f.InOutFunc := @FileNopProc;

    if f.Name[0] <> #0 then
    begin
      f.CloseFunc := @_CloseFile;
      case mode of
        1: begin
             Flags := O_APPEND or O_WRONLY;
             f.Mode := fmOutput;
           end;
        2: begin
             Flags := O_RDWR;
             f.Mode := fmInOut;
           end;
        3: begin
             Flags := O_CREAT or O_TRUNC or O_RDWR;
             f.Mode := fmInOut;
           end;
      else
        Flags := O_RDONLY;
        f.Mode := fmInput;
      end;

      uName := UTF8String(f.Name);
      f.Handle := __open(PAnsiChar(uName), Flags, FileAccessRights);
    end
    else  // stdin or stdout
    begin
      f.CloseFunc := @FileNopProc;
      if mode = 3 then
        f.Handle := STDOUT_FILENO
      else
        f.Handle := STDIN_FILENO;
    end;

    if f.Handle = -1 then
    begin
      f.Mode := fmClosed;
      InOutError;
    end;
  end
  else
    SetInOutRes(102);
end;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
const
  ShareTab: array [0..7] of Integer =
    (FILE_SHARE_READ OR FILE_SHARE_WRITE,  // OF_SHARE_COMPAT     0x00000000
     0,                                    // OF_SHARE_EXCLUSIVE  0x00000010
     FILE_SHARE_READ,                      // OF_SHARE_DENY_WRITE 0x00000020
     FILE_SHARE_WRITE,                     // OF_SHARE_DENY_READ  0x00000030
     FILE_SHARE_READ OR FILE_SHARE_WRITE,  // OF_SHARE_DENY_NONE  0x00000040
     0,0,0);
{$IFDEF CPU386}
asm
//->  EAX Pointer to file record
//    EDX Record size
//    ECX File mode

        PUSH     EBX
        PUSH     ESI
        PUSH     EDI

        MOV      ESI,EDX
        MOV      EDI,ECX
        XOR      EDX,EDX
        MOV      EBX,EAX

        MOV      DX,[EAX].TFileRec.Mode
        SUB      EDX,fmClosed
        JE       @@alreadyClosed
        CMP      EDX,fmInOut-fmClosed
        JA       @@notAssignedError

//  not yet closed: close it. File parameter is still in EAX

        CALL     [EBX].TFileRec.CloseFunc
        TEST     EAX,EAX
        JE       @@alreadyClosed
        CALL     SetInOutRes

@@alreadyClosed:

        MOV     [EBX].TFileRec.Mode,fmInOut
        MOV     [EBX].TFileRec.RecSize,ESI
        MOV     [EBX].TFileRec.CloseFunc,offset _CloseFile
        MOV     [EBX].TFileRec.InOutFunc,offset FileNopProc

        CMP     word ptr [EBX].TFileRec.Name,0
        JE      @@isCon

        MOV     EAX,GENERIC_READ OR GENERIC_WRITE
        MOV     DL,FileMode
        AND     EDX,070H
        SHR     EDX,4-2
        MOV     EDX,dword ptr [shareTab+EDX]
        MOV     ECX,CREATE_ALWAYS

        SUB     EDI,3
        JE      @@calledByRewrite

        MOV     ECX,OPEN_EXISTING
        INC     EDI
        JE      @@skip

        MOV     EAX,GENERIC_WRITE
        INC     EDI
        MOV     [EBX].TFileRec.Mode,fmOutput
        JE      @@skip

        MOV     EAX,GENERIC_READ
        MOV     [EBX].TFileRec.Mode,fmInput

@@skip:
@@calledByRewrite:

//  CreateFile(t.FileName, EAX, EDX, Nil, ECX, FILE_ATTRIBUTE_NORMAL, 0);

        PUSH     0
        PUSH     FILE_ATTRIBUTE_NORMAL
        PUSH     ECX
        PUSH     0
        PUSH     EDX
        PUSH     EAX
        LEA      EAX,[EBX].TFileRec.Name
        PUSH     EAX
        CALL     CreateFile
@@checkHandle:
        CMP      EAX,-1
        JZ       @@error

        MOV      [EBX].TFileRec.Handle,EAX
        JMP      @@exit

@@isCon:
        MOV      [EBX].TFileRec.CloseFunc,offset FileNopProc
        CMP      EDI,3
        JE       @@output
        PUSH     STD_INPUT_HANDLE
        JMP      @@1
@@output:
        PUSH     STD_OUTPUT_HANDLE
@@1:
        CALL     GetStdHandle
        JMP      @@checkHandle

@@notAssignedError:
        MOV      EAX,102
        JMP      @@errExit

@@error:
        MOV      [EBX].TFileRec.Mode,fmClosed
        CALL     GetLastError
@@errExit:
        CALL     SetInOutRes

@@exit:
        POP      EDI
        POP      ESI
        POP      EBX
end;
{$ELSE !CPU386}

var
   DesiredAccess: Integer;
   SharedMode: Integer;
   CreationDisposition: Integer;
begin
  Result := 0;
  if (f.Mode >= fmClosed) and (f.Mode <= fmInOut) then
  begin
    if f.Mode <> fmClosed then // not yet closed: close it
    begin
      Result := TFileIOFunc(f.CloseFunc)(f);
      if Result <> 0 then
        SetInOutRes(Result);
    end;

    if recSiz <= 0 then
      SetInOutRes(106);

    f.RecSize := recSiz;
    f.InOutFunc := @FileNopProc;

    if f.Name[0] <> #0 then
    begin
      f.CloseFunc := @_CloseFile;
      SharedMode := shareTab[(FileMode and $70) shr 4];
      case mode of
        1: begin
             CreationDisposition := OPEN_EXISTING;
             DesiredAccess := GENERIC_WRITE;
             f.Mode := fmOutput;
           end;
        2: begin
             CreationDisposition := OPEN_EXISTING;
             DesiredAccess := GENERIC_READ OR GENERIC_WRITE;
             f.Mode := fmInOut;
           end;
        3: begin
             DesiredAccess := GENERIC_READ OR GENERIC_WRITE;
             CreationDisposition := CREATE_ALWAYS;
             f.Mode := fmInOut;
           end;
      else
        CreationDisposition := OPEN_EXISTING;
        DesiredAccess := GENERIC_READ;
        f.Mode := fmInput;
      end;
      f.Handle := CreateFile(f.Name, DesiredAccess, SharedMode, nil,
                             CreationDisposition, FILE_ATTRIBUTE_NORMAL, 0);
    end
    else  // stdin or stdout
    begin
      f.CloseFunc := @FileNopProc;
      if mode = 3 then
        f.Handle := GetStdHandle(STD_OUTPUT_HANDLE)
      else
        f.Handle := GetStdHandle(STD_INPUT_HANDLE);;
    end;
    if f.Handle = -1 then
    begin
      f.Mode := fmClosed;
      InOutError;
    end;
  end
  else
    SetInOutRes(102);
end;
{$ENDIF !CPU386}
{$ENDIF MSWINDOWS}

function _ResetFile(var f: TFileRec; recSize: Longint): Integer;
var
  m: Byte;
begin
  m := FileMode and 3;
  if m > 2 then m := 2;
  Result := OpenFile(f, recSize, m);
end;

function _RewritFile(var f: TFileRec; recSize: Longint): Integer;
begin
  Result := OpenFile(f, recSize, 3);
end;



procedure _Seek(var f: TFileRec; recNum: Cardinal);
{$IFDEF MSWINDOWS}
{$IF not defined(CPU386) }
var
  Val64: UInt64;
  Val32Low: UInt32;
  Val32High: UInt32;
begin
  if (f.Mode >= fmInput) and (f.Mode <= fmInOut) then
  begin
    Val64 := UInt64(recNum) * f.RecSize;
    Val32High := UInt32(Val64 shr 32);
    Val32Low := UInt32(Val64);
    if Longint(SetFilePointer(f.Handle, Val32Low, @Val32High, FILE_BEGIN)) = -1 then
    begin
      if GetLastError <> 0 then
        SetInOutRes(103);
    end;
  end;
end;
{$ELSE}
asm
// -> EAX Pointer to file variable
//    EDX Record number

        MOV      ECX,EAX
        MOVZX    EAX,[EAX].TFileRec.Mode // check whether file is open
        SUB      EAX,fmInput
        CMP      EAX,fmInOut-fmInput
        JA       @@fileNotOpen

//  SetFilePointer(f.Handle, recNum*f.RecSize, FILE_BEGIN)
        PUSH     FILE_BEGIN    // pass dwMoveMethod
        MOV      EAX,[ECX].TFileRec.RecSize
        MUL      EDX
        PUSH     0           // pass lpDistanceToMoveHigh
        PUSH     EAX           // pass lDistanceToMove
        PUSH     [ECX].TFileRec.Handle   // pass hFile
        CALL     SetFilePointer          // get current position
        INC      EAX
        JZ       InOutError
        JMP      @@exit

@@fileNotOpen:
        MOV      EAX,103
        JMP      SetInOutRes

@@exit:
end;
{$IFEND}
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  if Longint(recNum) < 0 then  // Match windows behaviour. Only 0 .. MaxInt sizes are allowed.
    SetInOutRes(131);          // Negative seeks are dissablowed.
  if (f.Mode >= fmInput) and (f.Mode <= fmInOut) then
  begin

    if lseek(f.Handle, f.RecSize * recNum, SEEK_SET) = -1 then
      InOutError;
  end
  else
    SetInOutRes(103);
end;
{$ENDIF POSIX}




function _SeekEof(var t: TTextRec): Boolean;
{$IF not defined(CPU386)}
var
  c: Word;
begin
  while True do
  begin
    c := _ReadCharEx(t);
    if (c and $ff) > $20 then
    begin
      Dec(t.BufPos);
      Result := False;
      break;
    end;
    if ((c and $ff00) shr 8) = cEof then
    begin
      Result := True;
      break;
    end;
  end;
end;
{$ELSE}
asm
// -> EAX Pointer to text record
// <- AL  Boolean result
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH     EBX
        MOV      EBX,EAX
@@loop:
        MOV      EAX,EBX
        CALL     _ReadChar
        CMP      AL,' '
        JA       @@endloop
        CMP      AH,cEOF
        JE       @@eof
        JMP      @@loop
@@eof:
        MOV      AL,1
        JMP      @@exit

@@endloop:
        DEC      [EBX].TTextRec.BufPos
        XOR      AL,AL
@@exit:
        POP      EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
end;
{$IFEND}




function _SeekEoln(var t: TTextRec): Boolean;
{$IF not defined(CPU386)}
var
  c: Word;
begin
  while True do
  begin
    c := _ReadCharEx(t);
    if (c and $ff) > $20 then
    begin
      Dec(t.BufPos);
      Result := False;
      break;
    end;
    if ((c and $ff00) shr 8) = cEof then
    begin
      Result := True;
      break;
    end;
    if ((c and $ff) = cLF) or ((c and $ff) = cCR) then
    begin
      Dec(t.BufPos);
      Result := True;
      break;
    end;
  end;
end;
{$ELSE}
asm
// -> EAX Pointer to text record
// <- AL  Boolean result

{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH     EBX
        MOV      EBX,EAX
@@loop:
        MOV      EAX,EBX
        CALL     _ReadChar
        CMP      AL,' '
        JA       @@falseExit
        CMP      AH,cEOF
        JE       @@eof
        CMP      AL,cLF
        JE       @@trueExit
        CMP      AL,cCR
        JNE      @@loop

@@trueExit:
        MOV      AL,1
        JMP      @@exitloop

@@falseExit:
        XOR      AL,AL
@@exitloop:
        DEC      [EBX].TTextRec.BufPos
        JMP      @@exit

@@eof:
        MOV      AL,1
@@exit:
        POP EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
end;
{$IFEND}

procedure _SetTextBuf(var t: TTextRec; p: Pointer; size: Longint);
begin
  if size < 0 then
    Error(reRangeError);
  t.BufPtr := P;
  t.BufSize := size;
  t.BufPos := 0;
  t.BufEnd := 0;
end;



{$IFDEF PUREPASCAL}
function _StrLong(val, width: Longint): ShortString;
var
  I: Integer;
  sign: Longint;
  a: array [0..19] of AnsiChar;
  P: PAnsiChar;
begin
  // U-OK
  sign := val;
  val := Abs(val);
  I := 0;
  repeat
//    a[I] := Chr((val mod 10) + Ord('0'));
    a[I] := AnsiChar((val mod 10) + Ord('0'));
    Inc(I);
    val := val div 10;
  until val = 0;

  if sign < 0 then
  begin
    a[I] := '-';
    Inc(I);
  end;

  if width < I then
    width := I;
  if width > 255 then
    width := 255;
//  Result[0] := Chr(width);
  Result[0] := AnsiChar(width);
  P := @Result[1];
  while width > I do
  begin
    P^ := ' ';
    Inc(P);
    Dec(width);
  end;
  repeat
    Dec(I);
    P^ := a[I];
    Inc(P);
  until I <= 0;
end;
{$ELSE}
procedure _StrLong(val, width: Longint; s: PShortString);
asm
{       PROCEDURE _StrLong( val: Longint; width: Longint; VAR s: ShortString );
      ->EAX     Value
        EDX     Width
        ECX     Pointer to string       }

        PUSH    EBX             { VAR i: Longint;               }
        PUSH    ESI             { VAR sign : Longint;           }
        PUSH    EDI
        PUSH    EDX             { store width on the stack      }
        SUB     ESP,20          { VAR a: array [0..19] of Char; }

        MOV     EDI,ECX

        MOV     ESI,EAX         { sign := val                   }

        CDQ                     { val := Abs(val);  canned sequence }
        XOR     EAX,EDX
        SUB     EAX,EDX

        MOV     ECX,10
        XOR     EBX,EBX         { i := 0;                       }

@@repeat1:                      { repeat                        }
        XOR     EDX,EDX         {   a[i] := Chr( val MOD 10 + Ord('0') );}

        DIV     ECX             {   val := val DIV 10;          }

        ADD     EDX,'0'
        MOV     [ESP+EBX],DL
        INC     EBX             {   i := i + 1;                 }
        TEST    EAX,EAX         { until val = 0;                }
        JNZ     @@repeat1

        TEST    ESI,ESI
        JGE     @@2
        MOV     byte ptr [ESP+EBX],'-'
        INC     EBX
@@2:
        MOV     [EDI],BL        { s^++ := Chr(i);               }
        INC     EDI

        MOV     ECX,[ESP+20]    { spaceCnt := width - i;        }
        CMP     ECX,255
        JLE     @@3
        MOV     ECX,255
@@3:
        SUB     ECX,EBX
        JLE     @@repeat2       { for k := 1 to spaceCnt do s^++ := ' ';        }
        ADD     [EDI-1],CL
        MOV     AL,' '
        REP     STOSB

@@repeat2:                      { repeat                        }
        MOV     AL,[ESP+EBX-1]  {   s^ := a[i-1];               }
        MOV     [EDI],AL
        INC     EDI             {   s := s + 1                  }
        DEC     EBX             {   i := i - 1;                 }
        JNZ     @@repeat2       { until i = 0;                  }

        ADD     ESP,20+4
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF}

{$IFDEF PUREPASCAL}
function _Str0Long(val: Longint): ShortString;
begin
  Result := _StrLong(val, 0);
end;
{$ELSE}
procedure _Str0Long(val: Longint; s: PShortString);
begin
  _StrLong(val, 0, s);
end;
{$ENDIF}



procedure _Truncate(var f: TFileRec);
{$IFDEF MSWINDOWS}
{$IF not defined(CPU386)}
begin
  if (f.Mode = fmOutput) or (f.Mode = fmInOut) then
  begin
    if not SetEndOfFile(f.Handle) then
      InOutError;
  end
  else
    SetInOutRes(103);
end;
{$ELSE}
asm
// -> EAX Pointer to text or file variable

       MOVZX   EDX,[EAX].TFileRec.Mode   // check whether file is open
       SUB     EDX,fmInput
       CMP     EDX,fmInOut-fmInput
       JA      @@fileNotOpen

       PUSH    [EAX].TFileRec.Handle
       CALL    SetEndOfFile
       DEC     EAX
       JZ      @@exit
       JMP     InOutError

@@fileNotOpen:
       MOV     EAX,103
       JMP     SetInOutRes

@@exit:
end;
{$IFEND}
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  if (f.Mode and fmOutput) = fmOutput then  // fmOutput or fmInOut
  begin
    if ftruncate(f.Handle, lseek(f.Handle, 0, SEEK_CUR)) = -1 then
      InOutError;
  end
  else
    SetInOutRes(103);
end;
{$ENDIF POSIX}

// Hex : ( '$' | 'X' | 'x' | '0X' | '0x' ) [0-9A-Fa-f]*
// Dec : ( '+' | '-' )? [0-9]*

function _ValLong(const s: string; var code: Integer): Longint;
{$IFDEF PUREPASCAL}
var
  I, Len, Digit: Integer;
  Negative, Hex: Boolean;
begin
  // U-OK
  I := 1;
  code := -1;
  Result := 0;
  Negative := False;
  Hex := False;
  Len := Length(s);
  while (I <= Len) and (s[I] = ' ') do Inc(I);
  if I > Len then Exit;
  case s[I] of
    '$',
    'x',
    'X': begin
           Hex := True;
           Inc(I);
         end;

    '0': begin
           Hex := (Len > I) and ((s[I+1] = 'X') or (s[I+1] = 'x'));
           if Hex then Inc(I,2);
         end;
    '-': begin
           Negative := True;
           Inc(I);
         end;
    '+': Inc(I);
  end;
  if Hex then
    while I <= Len do
    begin
      // check for overflow
      if Result > (High(Result) shr 3) then
      begin
        code := I;
        Exit;
      end;
      case s[I] of
        '0'..'9': Result := Result * 16 + Ord(s[I]) - Ord('0');
        'a'..'f': Result := Result * 16 + Ord(s[I]) - Ord('a') + 10;
        'A'..'F': Result := Result * 16 + Ord(s[I]) - Ord('A') + 10;
      else
        code := I;
        Exit;
      end;
      Inc(I);
    end
  else
    while I <= Len do
    begin
      // check for overflow
      if Result > (High(Result) div 10) then
      begin
        code := I;
        Exit;
      end;
      Digit := Ord(s[I]) - Ord('0');
      if (Digit < 0) or (Digit > 9) then begin
         Code := I;
         Exit;
      end;
      Result := Result * 10 + Ord(s[I]) - Ord('0');
      Inc(I);
    end;
  if Negative then
    Result := -Result;
  code := 0;
end;
{$ELSE !PUREPASCAL}
asm
{       FUNCTION _ValLong( s: string; VAR code: Integer ) : Longint;        }
{     ->EAX     Pointer to string       }
{       EDX     Pointer to code result  }
{     <-EAX     Result                  }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        PUSH    EAX             { save for the error case       }

        TEST    EAX,EAX
        JE      @@empty

        XOR     EAX,EAX
        XOR     EBX,EBX
        MOV     EDI,07FFFFFFFH / 10     { limit }

@@blankLoop:
        MOV     BX,[ESI]
        ADD     ESI, 2
        CMP     BX,' '
        JE      @@blankLoop

@@endBlanks:
        MOV     CH,0
        CMP     BX,'-'
        JE      @@minus
        CMP     BX,'+'
        JE      @@plus

@@checkDollar:
        CMP     BX,'$'
        JE      @@dollar

        CMP     BX, 'x'
        JE      @@dollar
        CMP     BX, 'X'
        JE      @@dollar
        CMP     BX, '0'
        JNE     @@firstDigit
        MOV     BX, [ESI]
        ADD     ESI, 2
        CMP     BX, 'x'
        JE      @@dollar
        CMP     BX, 'X'
        JE      @@dollar
        TEST    BX, BX
        JE      @@endDigits
        JMP     @@digLoop

@@firstDigit:
        TEST    BX,BX
        JE      @@error

@@digLoop:
        SUB     BX,'0'
        CMP     BX,9
        JA      @@error
        CMP     EAX,EDI         { value > limit ?       }
        JA      @@overFlow
        LEA     EAX,[EAX+EAX*4]
        ADD     EAX,EAX
        ADD     EAX,EBX         { fortunately, we can't have a carry    }
        MOV     BX,[ESI]
        ADD     ESI, 2
        TEST    BX,BX
        JNE     @@digLoop

@@endDigits:
        DEC     CH
        JE      @@negate
        TEST    EAX,EAX
        JGE     @@successExit
        JMP     @@overFlow

@@empty:
        ADD     ESI, 2
        JMP     @@error

@@negate:
        NEG     EAX
        JLE     @@successExit
        JS      @@successExit           { to handle 2**31 correctly, where the negate overflows }

@@error:
@@overFlow:
        POP     EBX
        SUB     ESI,EBX
        JMP     @@exit

@@minus:
        INC     CH
@@plus:
        MOV     BX,[ESI]
        ADD     ESI, 2
        JMP     @@checkDollar

@@dollar:
        MOV     EDI,0FFFFFFFH
        MOV     BX,[ESI]
        ADD     ESI, 2
        TEST    BX,BX
        JZ      @@empty

@@hDigLoop:
        CMP     BX,'a'
        JB      @@upper
        SUB     BX,'a' - 'A'
@@upper:
        SUB     BX,'0'
        CMP     BX,9
        JBE     @@digOk
        SUB     BX,'A' - '0'
        CMP     BX,5
        JA      @@error
        ADD     BX,10
@@digOk:
        CMP     EAX,EDI
        JA      @@overFlow
        SHL     EAX,4
        ADD     EAX,EBX
        MOV     BX,[ESI]
        ADD     ESI, 2
        TEST    BX,BX
        JNE     @@hDigLoop

        DEC     CH
        JNE     @@successExit
        NEG     EAX

@@successExit:
        POP     ECX                     { saved copy of string pointer  }
        XOR     ESI,ESI         { signal no error to caller     }

@@exit:
        SHR     ESI, 1
        MOV     [EDX],ESI
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF}




function _WriteRec(var f: TFileRec; buffer: Pointer): Pointer;
{$IFDEF MSWINDOWS}
{$IF not defined(CPU386)}
var
  NumWritten: Cardinal;
begin
  if (f.Mode = fmOutput) or (f.Mode = fmInOut) then
  begin
    if WriteFile(f.Handle, Buffer, f.RecSize, NumWritten, nil) = 0 then
      SetInOutRes(GetLastError)
    else
    begin
      if f.RecSize <> NumWritten then
        SetInOutRes(101);
    end;
  end
  else
    SetInOutRes(5);
  Result := @F;
end;
{$ELSE}
asm
// -> EAX Pointer to file variable
//    EDX Pointer to buffer
// <- EAX Pointer to file variable
        PUSH    EBX

        MOV     EBX,EAX

        MOVZX   EAX,[EAX].TFileRec.Mode
        SUB     EAX,fmOutput
        CMP     EAX,fmInOut-fmOutput  // File must be fmInOut or fmOutput
        JA      @@fileNotOpen

//  WriteFile(f.Handle, buffer, f.RecSize, @result, Nil);

        PUSH    0                       // space for OS result
        MOV     EAX,ESP

        PUSH    0                       // pass lpOverlapped
        PUSH    EAX                     // pass @result
        PUSH    [EBX].TFileRec.RecSize  // pass nNumberOfBytesToRead
        PUSH    EDX                     // pass lpBuffer
        PUSH    [EBX].TFileRec.Handle   // pass hFile
        CALL    WriteFile
        POP     EDX                     // pop result
        DEC     EAX                     // check EAX = TRUE
        JNZ     @@error

        CMP     EDX,[EBX].TFileRec.RecSize  // result = f.RecSize ?
        JE      @@exit

@@writeError:
        MOV     EAX,101
        JMP     @@errExit

@@fileNotOpen:
        MOV     EAX,5
        JMP     @@errExit

@@error:
        CALL    GetLastError
@@errExit:
        CALL    SetInOutRes
@@exit:
        MOV     EAX,EBX
        POP     EBX
end;
{$IFEND}
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  Dummy: Integer;
begin
  _BlockWrite(f, Buffer, 1, Dummy);
  Result := @F;
end;
{$ENDIF POSIX}

// If the file is Output or ErrOutput std variable, try to open it
// Otherwise, runtime error.
function TryOpenForOutput(var t: TTextRec): Boolean;
begin
  if (@t = @Output) or (@t = @ErrOutput) then
  begin
    t.Flags := tfCRLF * Byte(DefaultTextLineBreakStyle);
    _RewritText(t);
  end;

  Result := t.Mode = fmOutput;
  if not Result then
    SetInOutRes(105);
end;




function _WriteBytes(var t: TTextRec; const b; cnt : Longint): Pointer;
{$IFDEF PUREPASCAL}
var
  Dest, Source: PAnsiChar;
  RemainingBytes: Longint;
  Temp: Integer;
begin
  // U-OK
  Result := @t;
  if (t.Mode <> fmOutput) and not TryOpenForOutput(t) then Exit;

  Source := Pointer(@b);
  Dest := t.BufPtr + t.BufPos;
  RemainingBytes := t.BufSize - t.BufPos;
  while RemainingBytes <= cnt do
  begin
    Inc(t.BufPos, RemainingBytes);
    Dec(cnt, RemainingBytes);
    Move(Source^, Dest^, RemainingBytes);
    Inc(Source, RemainingBytes);
    Temp := TTextIOFunc(t.InOutFunc)(t);
    if Temp <> 0 then
    begin
      SetInOutRes(Temp);
      Exit;
    end;
    Dest := t.BufPtr + t.BufPos;
    RemainingBytes := t.BufSize - t.BufPos;
  end;
  Inc(t.BufPos, cnt);
  Move(Source^, Dest^, cnt);
end;
{$ELSE !PUREPASCAL}
asm //StackAlignSafe
// -> EAX Pointer to file record
//    EDX Pointer to buffer
//    ECX Number of bytes to write
// <- EAX Pointer to file record

        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EDX

        CMP     [EAX].TTextRec.Mode,fmOutput
        JE      @@loop
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    TryOpenForOutput
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        TEST    AL,AL
        POP     ECX
        POP     EDX
        POP     EAX
        JE      @@exit

@@loop:
        MOV     EDI,[EAX].TTextRec.BufPtr
        ADD     EDI,[EAX].TTextRec.BufPos

//  remainingBytes = t.bufSize - t.bufPos

        MOV     EDX,[EAX].TTextRec.BufSize
        SUB     EDX,[EAX].TTextRec.BufPos

//  if (remainingBytes <= cnt)

        CMP     EDX,ECX
        JG      @@1

//  t.BufPos += remainingBytes, cnt -= remainingBytes

        ADD     [EAX].TTextRec.BufPos,EDX
        SUB     ECX,EDX

//  copy remainingBytes, advancing ESI

        PUSH    EAX
        PUSH    ECX
        MOV     ECX,EDX
        REP     MOVSB

{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    [EAX].TTextRec.InOutFunc
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        TEST    EAX,EAX
        JNZ     @@error

        POP     ECX
        POP     EAX
        JMP     @@loop

@@error:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    SetInOutRes
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        POP     ECX
        POP     EAX
        JMP     @@exit
@@1:
        ADD     [EAX].TTextRec.BufPos,ECX
        REP     MOVSB

@@exit:
        POP     EDI
        POP     ESI
end;
{$ENDIF}



function _WriteSpaces(var t: TTextRec; cnt: Longint): Pointer;
{$IFDEF PUREPASCAL}
const
  s64Spaces = '                                                                ';
begin
  Result := @t;
  while cnt > 64 do
  begin
    _WriteBytes(t, s64Spaces, 64);
    if InOutRes <> 0 then Exit;
    Dec(cnt, 64);
  end;
  if cnt > 0 then
    _WriteBytes(t, s64Spaces, cnt);
end;
{$ELSE !PUREPASCAL}
const
  spCnt = 64;
asm
// -> EAX Pointer to text record
//    EDX Number of spaces (<= 0: None)

        MOV     ECX,EDX
@@loop:
{$IFDEF PIC}
        LEA     EDX, [EBX] + offset @@spBuf
{$ELSE}
        MOV     EDX,offset @@spBuf
{$ENDIF}

        CMP     ECX,spCnt
        JLE     @@1

        SUB     ECX,spCnt
        PUSH    EAX
        PUSH    ECX
        MOV     ECX,spCnt
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    _WriteBytes
        CALL    SysInit.@GetTLS
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        CMP     [EAX].InOutRes,0
        JNE     @@error
        POP     ECX
        POP     EAX
        JMP     @@loop

@@error:
        POP ECX
        POP EAX
        JMP     @@exit

@@spBuf:  // 64 spaces
        DB '                                                                ';
@@1:
        TEST  ECX,ECX
        JG  _WriteBytes
@@exit:
end;
{$ENDIF}




function _Write0Char(var t: TTextRec; c: AnsiChar): Pointer;
{$IFDEF PUREPASCAL}
var
  Temp: Integer;
begin
  Result := @t;
  if (t.Mode <> fmOutput) and not TryOpenForOutput(t) then Exit;

  if t.BufPos >= t.BufSize then
  begin
    Temp := TTextIOFunc(t.InOutFunc)(t);
    if Temp <> 0 then
    begin
      SetInOutRes(Temp);
      Exit;
    end;
  end;

  t.BufPtr[t.BufPos] := c;
  Inc(t.BufPos);
end;
{$ELSE !PUREPASCAL}
asm
// -> EAX Pointer to text record
//  DL  Character

        CMP     [EAX].TTextRec.Mode,fmOutput
        JE      @@loop
        PUSH    EAX
        PUSH    EDX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    TryOpenForOutput
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        TEST    AL,AL
        POP     EDX
        POP     EAX
        JNE     @@loop
        JMP     @@exit

@@flush:
        PUSH    EAX
        PUSH    EDX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    [EAX].TTextRec.InOutFunc
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        TEST    EAX,EAX
        JNZ     @@error
        POP     EDX
        POP     EAX
        JMP     @@loop

@@error:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    SetInOutRes
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        POP     EDX
        POP     EAX
        JMP     @@exit

@@loop:
        MOV     ECX,[EAX].TTextRec.BufPos
        CMP     ECX,[EAX].TTextRec.BufSize
        JGE     @@flush

        ADD     ECX,[EAX].TTextRec.BufPtr
        MOV     [ECX],DL

        INC     [EAX].TTextRec.BufPos
@@exit:
end;
{$ENDIF}

function _WriteChar(var t: TTextRec; c: AnsiChar; width: Integer): Pointer;
begin
  _WriteSpaces(t, width-1);
  Result := _WriteBytes(t, c, 1);
end;

function _WriteBool(var t: TTextRec; val: Boolean; width: Longint): Pointer;
const
  BoolStrs: array [Boolean] of ShortString = ('FALSE', 'TRUE');
begin
  Result := _WriteString(t, BoolStrs[val], width);
end;

function _Write0Bool(var t: TTextRec; val: Boolean): Pointer;
begin
  Result := _WriteBool(t, val, 0);
end;

function _WriteLong(var t: TTextRec; val, width: Longint): Pointer;
var
  S: string[31];
begin
  Str(val:0, S);
  Result := _WriteString(t, S, width);
end;

function _Write0Long(var t: TTextRec; val: Longint): Pointer;
begin
  Result := _WriteLong(t, val, 0);
end;

function _Write0String(var t: TTextRec; const s: ShortString): Pointer;
begin
  Result := _WriteBytes(t, S[1], Byte(S[0]));
end;

function _WriteString(var t: TTextRec; const s: ShortString; width: Longint): Pointer;
begin
  _WriteSpaces(t, Width - Byte(S[0]));
  Result := _WriteBytes(t, S[1], Byte(S[0]));
end;

function _Write0CString(var t: TTextRec; s: PAnsiChar): Pointer;
begin
  Result := _WriteCString(t, s, 0);
end;

function _WriteCString(var t: TTextRec; s: PAnsiChar; width: Longint): Pointer;
var
  len: Longint;
begin
{$IFDEF MSWINDOWS}
  len := _strlenA(s);
{$ENDIF}
{$IFDEF POSIX}
  if Assigned(s) then
    len := strlen(s)
  else
    len := 0;
{$ENDIF}
  _WriteSpaces(t, width - len);
  Result := _WriteBytes(t, s^, len);
end;

function _Write0LString(var t: TTextRec; const s: AnsiString): Pointer;
begin
  Result := _WriteLString(t, s, 0);
end;





function _WriteLString(var t: TTextRec; const s: AnsiString; width: Longint): Pointer;
{$IFDEF PUREPASCAL}
var
  i: Integer;
begin
  i := Length(s);
  _WriteSpaces(t, width - i);
  Result := _WriteBytes(t, s[1], i);
end;
{$ELSE !PUREPASCAL}
asm
        { ->    EAX     Pointer to text record  }
        {       EDX     Pointer to AnsiString   }
        {       ECX     Field width             }
        PUSH    EBX
        PUSH    ESI

        MOV     EBX, EDX

        MOV     EDX, ECX
        XOR     ECX, ECX
        TEST    EBX, EBX
        JE      @@skip
        MOV     ECX, [EBX-skew].StrRec.length
        SUB     EDX, ECX
@@skip:
        PUSH    ECX
{$IFDEF PIC}
        PUSH    EBX
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX, EAX
        POP     EAX
{$ENDIF PIC}
        CALL    _WriteSpaces
{$IFDEF PIC}
        POP     EBX
{$ENDIF PIC}
        POP     ECX

        MOV     EDX, EBX
        POP     ESI
        POP     EBX
        JMP     _WriteBytes
end;
{$ENDIF !PUREPASCAL}

function _Write0UString(var t: TTextRec; const s: UnicodeString): Pointer;
begin
  Result := _WriteUString(t, s, 0);
end;

function _WriteUString(var t: TTextRec; const s: UnicodeString; width: Longint): Pointer;
var
  i: Integer;
begin
  // !!! FIXME
  i := Length(s);
  _WriteSpaces(t, width - i);
  Result := _WriteLString(t, AnsiString(s), 0);
end;

function _Write0WString(var t: TTextRec; const s: WideString): Pointer;
begin
  Result := _WriteWString(t, s, 0);
end;

function _WriteWString(var t: TTextRec; const s: WideString; width: Longint): Pointer;
var
  i: Integer;
begin
  i := Length(s);
  _WriteSpaces(t, width - i);
  Result := _WriteLString(t, AnsiString(s), 0);
end;

function _Write0WCString(var t: TTextRec; s: PWideChar): Pointer;
begin
  Result := _WriteWCString(t, s, 0);
end;

function _WriteWCString(var t: TTextRec; s: PWideChar; width: Longint): Pointer;
var
  i: Integer;
begin
  i := 0;
  if (s <> nil) then
    while s[i] <> #0 do
      Inc(i);

  _WriteSpaces(t, width - i);
  Result := _WriteLString(t, AnsiString(s), 0);
end;

function _Write0WChar(var t: TTextRec; c: WideChar): Pointer;
begin
  Result := _WriteWChar(t, c, 0);
end;

function _WriteWChar(var t: TTextRec; c: WideChar; width: Integer): Pointer;
begin
  _WriteSpaces(t, width - 1);
  Result := _WriteLString(t, AnsiString(c), 0);
end;

function _WriteVariant(var T: TTextRec; const V: TVarData; Width: Integer): Pointer;
var
  S: AnsiString;
begin
  if Assigned(VarToLStrProc) then
  begin
    VarToLStrProc(S, V);
    _WriteLString(T, S, Width);
  end
  else
    Error(reVarInvalidOp);
  Result := @T;
end;

function _Write0Variant(var T: TTextRec; const V: TVarData): Pointer;
begin
  Result := _WriteVariant(T, V, 0);
end;




{$IF not defined(CPU386)}
function _Write2Ext(var t: Text; val: Extended; width, prec: Longint): Pointer;
var
  s: ShortString;
begin
  s := _Str2Ext(val, width, prec);
  Result := _WriteString(TTextRec(t), s, width);
end;
{$ELSE}
procedure _Write2Ext;
asm
{       PROCEDURE _Write2Ext( VAR t: Text; val: Extended; width, prec: Longint);
      ->EAX     Pointer to file record
        [ESP+4] Extended value
        EDX     Field width
        ECX     precision (<0: scientific, >= 0: fixed point)   }

        FLD     tbyte ptr [ESP+4]       { load value            }
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF}
        SUB     ESP,256                 { VAR s: String;        }

        PUSH    EAX
        PUSH    EDX

{       Str( val, width, prec, s );     }

        SUB     ESP,12
        FSTP    tbyte ptr [ESP]         { pass value                    }
        MOV     EAX,EDX                 { pass field width              }
        MOV     EDX,ECX                 { pass precision                }
        LEA     ECX,[ESP+8+12]          { pass destination string       }
        CALL    _Str2Ext

{       Write( t, s, width );   }

        POP     ECX                     { pass width    }
        POP     EAX                     { pass text     }
        MOV     EDX,ESP                 { pass string   }
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF}
        CALL    _WriteString

        ADD     ESP,256
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF}
        RET     12
end;
{$IFEND}




{$IF not defined(CPU386)}
function _Write1Ext(var t: Text; val: Extended; width: Longint): Pointer;
begin
  Result := _Write2Ext(t, val, width, -1);
end;
{$ELSE}
procedure _Write1Ext;
asm
{       PROCEDURE _Write1Ext( VAR t: Text; val: Extended; width: Longint);
  ->    EAX     Pointer to file record
        [ESP+4] Extended value
        EDX     Field width             }

        OR      ECX,-1
        JMP     _Write2Ext
end;
{$IFEND}




{$IF not defined(CPU386)}
function _Write0Ext(var t: Text; val: Extended): Pointer;
begin
  Result := _Write2Ext(t, val, 23, -1);
end;
{$ELSE}
procedure _Write0Ext;
asm
{       PROCEDURE _Write0Ext( VAR t: Text; val: Extended);
      ->EAX     Pointer to file record
        [ESP+4] Extended value  }

        MOV     EDX,23  { field width   }
        OR      ECX,-1
        JMP     _Write2Ext
end;
{$IFEND}

function _WriteLn(var t: TTextRec): Pointer;
var
  Buf: array [0..1] of AnsiChar;
begin
  if (t.flags and tfCRLF) <> 0 then
  begin
    Buf[0] := AnsiChar(#13);
    Buf[1] := AnsiChar(#10);
    Result := _WriteBytes(t, Buf, 2);
  end
  else
  begin
    Buf[0] := AnsiChar(#10);
    Result := _WriteBytes(t, Buf, 1);
  end;
  _Flush(t);
end;

procedure __CToPasStr(Dest: PShortString; const Source: PAnsiChar);
begin
  __CLenToPasStr(Dest, Source, 255);
end;



procedure __CLenToPasStr(Dest: PShortString; const Source: PAnsiChar; MaxLen: Integer);
{$IFDEF PUREPASCAL}
var
  I: Integer;
begin
  I := 0;
  if Source <> nil then
  begin
    if MaxLen > 255 then MaxLen := 255;
    while (Source[I] <> #0) and (I <= MaxLen) do
    begin
      Dest^[I+1] := Source[I];
      Inc(I);
    end;
    if I > 0 then Dec(I);
  end;
  Byte(Dest^[0]) := I;
end;
{$ELSE !PUREPASCAL}
asm
{     ->EAX     Pointer to destination  }
{       EDX     Pointer to source       }
{       ECX     cnt                     }

        PUSH    EBX
        PUSH    EAX             { save destination      }

        TEST    EDX,EDX
        JZ      @@nilStr
        CMP     ECX,255
        JBE     @@loop
        MOV     ECX,255
@@loop:
        MOV     BL,[EDX]        { ch = *src++;          }
        INC     EDX
        TEST    BL,BL           { if (ch == 0) break    }
        JE      @@endLoop
        INC     EAX             { *++dest = ch;         }
        MOV     [EAX],BL
        DEC     ECX             { while (--cnt != 0)    }
        JNZ     @@loop

@@endLoop:
        POP     EDX
        SUB     EAX,EDX

@@setLength:
        MOV     [EDX],AL
        POP     EBX
        RET

@@nilStr:
        POP     EDX
        XOR     EAX,EAX
        JMP     @@setLength
end;
{$ENDIF !PUREPASCAL}

procedure __ArrayToPasStr(Dest: PShortString; const Source: PAnsiChar; Len: Integer);
begin
  if Len > 255 then Len := 255;
  Byte(Dest^[0]) := Len;
  Move(Source^, Dest^[1], Len);
end;

procedure __PasToCStr(const Source: PShortString; const Dest: PAnsiChar);
begin
  Move(Source^[1], Dest^, Byte(Source^[0]));
  Dest[Byte(Source^[0])] := #0;
end;


{ ----------------------------------------------------- }
{       Compiler helper for set type support            }
{ ----------------------------------------------------- }

{$IF not defined(CPU386)}

procedure _SetElem(var Dest {:Set}; Elem, Size: Integer);
var
  P: PByte;
  I: Integer;
begin
  P := @Dest;
  for I := 0 to Size - 1 do
    P[I] := 0;
  if (Elem >= 0) and ((Elem div 8) < Size) then
    P[Elem div 8] := 1 shl (Elem mod 8);
end;
{$ELSE}


procedure       _SetElem;
asm
        {       PROCEDURE _SetElem( VAR d: SET; elem, size: Byte);      }
        {       EAX     =       dest address                            }
        {       DL      =       element number                          }
        {       CL      =       size of set                             }

        PUSH    EBX
        PUSH    EDI

        MOV     EDI,EAX

        XOR     EBX,EBX { zero extend set size into ebx }
        MOV     BL,CL
        MOV     ECX,EBX { and use it for the fill       }

        XOR     EAX,EAX { for zero fill                 }
        REP     STOSB

        SUB     EDI,EBX { point edi at beginning of set again   }

        INC     EAX             { eax is still zero - make it 1 }
        MOV     CL,DL
        ROL     AL,CL   { generate a mask               }
        SHR     ECX,3   { generate the index            }
        CMP     ECX,EBX { if index >= siz then exit     }
        JAE     @@exit
        OR      [EDI+ECX],AL{ set bit                   }

@@exit:
        POP     EDI
        POP     EBX
end;
{$IFEND}

{$IF not defined(CPU386)}

procedure _SetRange(Lo, Hi, Size: Integer; var Dest {:Set});
var
  P: PByte;
  I: Integer;
  LoIndex, HiIndex: Integer;
  LoMask, HiMask: Byte;
begin
  P := @dest;
  for I := 0 to Size - 1 do
    P[I] := 0;
  if Hi >= Size * 8 then
    Hi := Size * 8 - 1;
  if Lo < 0 then
    Lo := 0;
  if Lo <= Hi then
  begin
    LoMask := $ff shl (Lo mod 8);
    LoIndex := Lo div 8;
    HiMask := LongWord($FF) shr (7 - (Hi mod 8));
    HiIndex := Hi div 8;
    P[LoIndex] := LoMask;
    for I := LoIndex+1 to HiIndex do
      P[I] := $ff;
    P[HiIndex] := P[HiIndex] and HiMask;
  end;
end;
{$ELSE}


procedure _SetRange;
asm
{       PROCEDURE _SetRange( lo, hi, size: Byte; VAR d: SET );  }
{ ->    AL      low limit of range      }
{       DL      high limit of range     }
{       ECX     Pointer to set          }
{       AH      size of set             }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        XOR     EBX,EBX { EBX = set size                }
        MOV     BL,AH
        MOVZX   ESI,AL  { ESI = low zero extended       }
        MOVZX   EDX,DL  { EDX = high zero extended      }
        MOV     EDI,ECX

{       clear the set                                   }

        MOV     ECX,EBX
        XOR     EAX,EAX
        REP     STOSB

{       prepare for setting the bits                    }

        SUB     EDI,EBX { point EDI at start of set     }
        SHL     EBX,3   { EBX = highest bit in set + 1  }
        CMP     EDX,EBX
        JB      @@inrange
        LEA     EDX,[EBX-1]     { ECX = highest bit in set      }

@@inrange:
        CMP     ESI,EDX { if lo > hi then exit;         }
        JA      @@exit

        DEC     EAX     { loMask = 0xff << (lo & 7)             }
        MOV     ECX,ESI
        AND     CL,07H
        SHL     AL,CL

        SHR     ESI,3   { loIndex = lo >> 3;            }

        MOV     CL,DL   { hiMask = 0xff >> (7 - (hi & 7));      }
        NOT     CL
        AND     CL,07
        SHR     AH,CL

        SHR     EDX,3   { hiIndex = hi >> 3;            }

        ADD     EDI,ESI { point EDI to set[loIndex]     }
        MOV     ECX,EDX
        SUB     ECX,ESI { if ((inxDiff = (hiIndex - loIndex)) == 0)     }
        JNE     @@else

        AND     AL,AH   { set[loIndex] = hiMask & loMask;       }
        MOV     [EDI],AL
        JMP     @@exit

@@else:
        STOSB           { set[loIndex++] = loMask;      }
        DEC     ECX
        MOV     AL,0FFH { while (loIndex < hiIndex)     }
        REP     STOSB   {   set[loIndex++] = 0xff;      }
        MOV     [EDI],AH        { set[hiIndex] = hiMask;        }

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$IFEND}

{$IF not defined(CPU386)}

function _SetEq(L, R: Pointer; Size: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Size - 1 do
    if PByte(L)[I] <> PByte(R)[I] then Exit;
  Result := True;
end;
{$ELSE}


procedure _SetEq;
asm
{       FUNCTION _SetEq( CONST l, r: Set; size: Byte): ConditionCode;   }
{       EAX     =       left operand    }
{       EDX     =       right operand   }
{       CL      =       size of set     }

        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX

        AND     ECX,0FFH

@@Loop:
        DEC     ECX
        JZ      @@ByteCheck
        MOVZX   EAX,WORD PTR [ESI+ECX-1]
        MOVZX   EDX,WORD PTR [EDI+ECX-1]
        CMP     EAX,EDX
        JNE     @@Leave
        DEC     ECX
        JNZ     @@Loop
@@Leave:

        POP     EDI
        POP     ESI
        RET

@@ByteCheck:
        MOV     AL,[ESI+ECX]
        MOV     DL,[EDI+ECX]
        CMP     AL,DL
        JNE     @@Leave
        OR      ECX,ECX // set zero flag

        POP     EDI
        POP     ESI
        RET
end;
{$IFEND}

{$IF not defined(CPU386)}

function _SetLe(L, R: Pointer; Size: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Size - 1 do
    if (PByte(L)[I] and not PByte(R)[I]) <> 0 then Exit;
  Result := True;
end;
{$ELSE}


procedure _SetLe;
asm
{       FUNCTION _SetLe( CONST l, r: Set; size: Byte): ConditionCode;   }
{       EAX     =       left operand            }
{       EDX     =       right operand           }
{       CL      =       size of set (>0 && <= 32)       }

@@loop:
        MOV     CH,[EDX]
        NOT     CH
        AND     CH,[EAX]
        JNE     @@exit
        INC     EDX
        INC     EAX
        DEC     CL
        JNZ     @@loop
@@exit:
end;
{$IFEND}

{$IF not defined(CPU386)}

procedure _SetIntersect(var Dest {:Set}; Src: Pointer{PSet}; Size: Integer);
var
  I: Integer;
  PD, PS: PByte;
begin
  PD := PByte(@Dest);
  PS := PByte(Src);
  for I := 0 to Size - 1 do
    PD[I] := PD[I] and PS[I];
end;
{$ELSE}


procedure _SetIntersect;
asm
{       PROCEDURE _SetIntersect( VAR dest: Set; CONST src: Set; size: Byte);}
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       CL      =       size of set (0 < size <= 32)    }

@@loop:
        MOV     CH,[EDX]
        INC     EDX
        AND     [EAX],CH
        INC     EAX
        DEC     CL
        JNZ     @@loop
end;
{$IFEND}

{$IF not defined(CPU386)}

procedure _SetIntersect3(var Dest {:Set}; L, R: Pointer{PSet}; Size: Integer);
var
  I: Integer;
  PD, PL, PR: PByte;
begin
  PD := PByte(@Dest);
  PL := PByte(L);
  PR := PByte(R);
  for I := 0 to Size - 1 do
    PD[I] := PL[I] and PR[I];
end;
{$ELSE}


procedure _SetIntersect3;
asm
{       PROCEDURE _SetIntersect3( VAR dest: Set; CONST src: Set; size: Longint; src2: Set);}
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       ECX     =       size of set (0 < size <= 32)    }
{       [ESP+4] = 2nd source operand                    }

        PUSH    EBX
        PUSH    ESI
        MOV     ESI,[ESP+8+4]
@@loop:
        MOV     BL,[EDX+ECX-1]
        AND     BL,[ESI+ECX-1]
        MOV     [EAX+ECX-1],BL
        DEC     ECX
        JNZ     @@loop

        POP     ESI
        POP     EBX
end;
{$IFEND}

{$IF not defined(CPU386)}

procedure _SetUnion(var Dest {:Set}; Src: Pointer{PSet}; Size: Integer);
var
  I: Integer;
  PD, PS: PByte;
begin
  PD := PByte(@Dest);
  PS := PByte(Src);
  for I := 0 to Size - 1 do
    PD[I] := PD[I] or PS[I];
end;
{$ELSE}


procedure _SetUnion;
asm
{       PROCEDURE _SetUnion( VAR dest: Set; CONST src: Set; size: Byte);        }
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       CL      =       size of set (0 < size <= 32)    }

@@loop:
        MOV     CH,[EDX]
        INC     EDX
        OR      [EAX],CH
        INC     EAX
        DEC     CL
        JNZ     @@loop
end;
{$IFEND}

{$IF not defined(CPU386)}

procedure _SetUnion3(var Dest {:Set}; L, R: Pointer{PSet}; Size: Integer);
var
  I: Integer;
  PD, PL, PR: PByte;
begin
  PD := PByte(@Dest);
  PL := PByte(L);
  PR := PByte(R);
  for I := 0 to Size - 1 do
    PD[I] := PL[I] or PR[I];
end;
{$ELSE}


procedure _SetUnion3;
asm
{       PROCEDURE _SetUnion3( VAR dest: Set; CONST src: Set; size: Longint; src2: Set);}
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       ECX     =       size of set (0 < size <= 32)    }
{ [ESP+4] = 2nd source operand    }

      PUSH  EBX
      PUSH  ESI
      MOV   ESI,[ESP+8+4]
@@loop:
      MOV   BL,[EDX+ECX-1]
      OR    BL,[ESI+ECX-1]
      MOV   [EAX+ECX-1],BL
      DEC   ECX
      JNZ   @@loop

      POP   ESI
      POP   EBX
end;
{$IFEND}

{$IF not defined(CPU386)}

procedure _SetSub(var Dest {:set}; const Src {:Set}; Size: Integer);
var
  I: Integer;
  PD, PS: PByte;
begin
  PD := PByte(@Dest);
  PS := PByte(Src);
  for I := 0 to Size - 1 do
    PD[I] := PD[I] and not PS[I];
end;
{$ELSE}


procedure _SetSub;
asm
{       PROCEDURE _SetSub( VAR dest: Set; CONST src: Set; size: Byte);  }
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       CL      =       size of set (0 < size <= 32)    }

@@loop:
        MOV     CH,[EDX]
        NOT     CH
        INC     EDX
        AND     [EAX],CH
        INC     EAX
        DEC     CL
        JNZ     @@loop
end;
{$IFEND}

{$IF not defined(CPU386)}

procedure _SetSub3(var Dest {:set}; L, R: Pointer{PSet}; Size: Integer);
var
  I: Integer;
  PD, PL, PR: PByte;
begin
  PD := PByte(@Dest);
  PL := PByte(L);
  PR := PByte(R);
  for I := 0 to Size - 1 do
    PD[I] := PL[I] and not PR[I];
end;
{$ELSE}


procedure _SetSub3;
asm
{       PROCEDURE _SetSub3( VAR dest: Set; CONST src: Set; size: Longint; src2: Set);}
{       EAX     =       destination operand             }
{       EDX     =       source operand                  }
{       ECX     =       size of set (0 < size <= 32)    }
{       [ESP+4] = 2nd source operand                    }

        PUSH    EBX
        PUSH    ESI
        MOV     ESI,[ESP+8+4]
@@loop:
        MOV     BL,[ESI+ECX-1]
        NOT     BL
        AND     BL,[EDX+ECX-1]
        MOV     [EAX+ECX-1],BL
        DEC     ECX
        JNZ     @@loop

        POP     ESI
        POP     EBX
end;
{$IFEND}

{$IF not defined(CPU386)}
procedure _SetExpand(Src: Pointer{PSet}; var Dest {:Set}; Lo, Hi: Integer);
var
  I: Integer;
  PD, PS: PByte;
begin
  PD := PByte(@Dest);
  PS := PByte(Src);
  for I := 0 to Lo - 1 do
    PD[I] := 0;
  for I := Lo to Hi - 1 do
    PD[I] := PS[I - Lo];
  for I := Hi to 31 do
    PD[I] := 0;
end;
{$ELSE}


procedure _SetExpand;
asm
{       PROCEDURE _SetExpand( CONST src: Set; VAR dest: Set; lo, hi: Byte);     }
{     ->EAX     Pointer to source (packed set)          }
{       EDX     Pointer to destination (expanded set)   }
{       CH      high byte of source                     }
{       CL      low byte of source                      }

{       algorithm:              }
{       clear low bytes         }
{       copy high-low+1 bytes   }
{       clear 31-high bytes     }

        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX

        MOV     EDX,ECX { save low, high in dl, dh      }
        XOR     ECX,ECX
        XOR     EAX,EAX

        MOV     CL,DL   { clear low bytes               }
        REP     STOSB

        MOV     CL,DH   { copy high - low bytes }
        SUB     CL,DL
        REP     MOVSB

        MOV     CL,32   { copy 32 - high bytes  }
        SUB     CL,DH
        REP     STOSB

        POP     EDI
        POP     ESI
end;
{$IFEND}

{$IF not defined(CPU386)}

// -> val : 0 <= val < 10.0
//    digCnt : Count of digits to generate
//    digBuf : Pointer to digit buffer
procedure _EmitDigits(val: Extended; digCnt: Integer; digBuf: PAnsiChar);
const
  tenE17: Double = 1e17;
  tenE18: Double = 1e18;
var
  bcdBuf: array [0..9] of Byte;
begin
  while digCnt > 0 do
  begin
    Dec(digCnt);
    digBuf[digCnt] := '0';
  end;
end;
{$ELSE}



procedure _EmitDigits;
const
  tenE17: Double = 1e17;
  tenE18: Double = 1e18;
asm
// -> FST(0)  Value, 0 <= value < 10.0
//  EAX Count of digits to generate
//  EDX Pointer to digit buffer

        PUSH    EBX
{$IFDEF PIC}
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX,EAX
        POP     EAX
{$ELSE !PIC}
        XOR     EBX,EBX
{$ENDIF !PIC}
        PUSH    EDI
        MOV     EDI,EDX
        MOV     ECX,EAX

        SUB     ESP,10              // VAR bcdBuf: array [0..9] of Byte
        MOV     byte ptr [EDI],'0'  // digBuf[0] := '0'//
        FMUL    qword ptr [EBX] + offset tenE17 // val := Round(val*1e17);
        FRNDINT

        FCOM    qword ptr [EBX] + offset tenE18 // if val >= 1e18 then
        FSTSW   AX
        SAHF
        JB      @@1

        FSUB    qword ptr [EBX] + offset tenE18  //   val := val - 1e18;
        MOV     byte ptr [EDI],'1'               //   digBuf[0] := '1';
@@1:
        FBSTP   tbyte ptr [ESP] // store packed bcd digits in bcdBuf

        MOV     EDX,8
        INC     EDI

@@2:
        WAIT
        MOV     AL,[ESP+EDX]  // unpack 18 bcd digits in 9 bytes
        MOV     AH,AL         // into 9 words = 18 bytes
        SHR     AL,4
        AND     AH,0FH
        ADD     AX,'00'
        STOSW
        DEC     EDX
        JNS     @@2

        SUB     ECX,18        // we need at least digCnt digits
        JL      @@3           // we have generated 18

        MOV     AL,'0'        // if this is not enough, append zeroes
        REP     STOSB
        JMP     @@4           // in this case, we don't need to round

@@3:
        ADD     EDI,ECX       // point EDI to the round digit
        CMP     byte ptr [EDI],'5'
        JL      @@4
@@5:
        DEC     EDI
        INC     byte ptr [EDI]
        CMP     byte ptr [EDI],'9'
        JLE     @@4
        MOV     byte ptr [EDI],'0'
        JMP     @@5

@@4:
        ADD     ESP,10
        POP     EDI
        POP     EBX
end;
{$IFEND}




{$IF not defined(CPU386)}
function _ScaleExt(var val: Extended): Integer;
begin
  Result := 0;
end;
{$ELSE}
procedure _ScaleExt;
asm
// -> FST(0)  Value
// <- EAX exponent (base 10)
//  FST(0)  Value / 10**eax
// PIC safe - uses EBX, but only call is to _POW10, which fixes up EBX itself

        PUSH    EBX
        SUB     ESP,12

        XOR     EBX,EBX

@@normLoop: // loop necessary for denormals

        FLD     ST(0)
        FSTP    tbyte ptr [ESP]
        MOV     AX,[ESP+8]
        TEST    AX,AX
        JE      @@testZero
@@cont:
        SUB     AX,3FFFH
        MOV     DX,4D10H  // log10(2) * 2**16
        IMUL    DX
        MOVSX   EAX,DX    // exp10 = exp2 * log10(2)
        NEG     EAX
        JE      @@exit
        SUB     EBX,EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP,12
{$ENDIF}
        CALL    _Pow10
{$IFDEF ALIGN_STACK}
        ADD     ESP,12
{$ENDIF}
        JMP     @@normLoop

@@testZero:
        CMP     dword ptr [ESP+4],0
        JNE     @@cont
        CMP     dword ptr [ESP+0],0
        JNE     @@cont

@@exit:
        ADD     ESP,12
        MOV     EAX,EBX
        POP     EBX
end;
{$IFEND}

const
  Ten: Double = 10.0;
  NanStr: String[3] = 'Nan';
  PlusInfStr: String[4] = '+Inf';
  MinInfStr: String[4] = '-Inf';





{$IF not defined(CPU386)}
function _Str2Ext(val: Extended; width, precision: LongInt): ShortString;
begin
  Result := '999';
end;
{$ELSE}
procedure _Str2Ext;//( val: Extended; width, precision: Longint; var s: String );
const
  MAXDIGS = 256;
asm
// -> [ESP+4] Extended value
//  EAX Width
//  EDX Precision
//  ECX Pointer to string

      FLD     tbyte ptr [ESP+4]

      PUSH    EBX
      PUSH    ESI
      PUSH    EDI
      MOV     EBX,EAX
      MOV     ESI,EDX
      PUSH    ECX         // save string pointer
      SUB     ESP,MAXDIGS // VAR digBuf: array [0..MAXDIGS-1] of Char

      //  limit width to 255
      CMP     EBX,255     // if width > 255 then width := 255;
      JLE     @@1
      MOV     EBX,255
@@1:
      //  save sign bit in bit 0 of EDI, take absolute value of val,
      //  check for Nan and infinity.
      FLD     ST(0)
      FSTP    tbyte ptr [ESP]
      XOR     EAX,EAX
      MOV     AX,word ptr [ESP+8]
      MOV     EDI,EAX
      SHR     EDI,15
      AND     AX,7FFFH
      CMP     AX,7FFFH
      JE      @@nanInf
      FABS

      //  if precision < 0 then do scientific else do fixed;
      TEST    ESI,ESI
      JGE     @@fixed

      //  the following call finds a decimal exponent and a reduced
      //  mantissa such that val = mant * 10**exp
{$IFDEF ALIGN_STACK}
      SUB     ESP,12
{$ENDIF}
      CALL    _ScaleExt   // val is FST(0), exp is EAX
{$IFDEF ALIGN_STACK}
      ADD     ESP,12
{$ENDIF}

@@scientific:
      //  for scientific notation, we have width - 8 significant digits
      //  however, we can not have less than 2 or more than 18 digits.

      MOV     ESI,EBX     // digCnt := width - 8;
      SUB     ESI,8
      CMP     ESI,2       // if digCnt < 2 then digCnt := 2
      JGE     @@2
      MOV     ESI,2
      JMP     @@3
@@2:
      CMP     ESI,18      // else if digCnt > 18 then digCnt := 18;
      JLE     @@3
      MOV     ESI,18

@@3:
      //  _EmitDigits( val, digCnt, digBuf )
      MOV     EDX,ESP     // pass digBuf
      PUSH    EAX         // save exponent
      MOV     EAX,ESI     // pass digCnt
{$IFDEF ALIGN_STACK}
      SUB     ESP,8
{$ENDIF}
      CALL    _EmitDigits // convert val to ASCII
{$IFDEF ALIGN_STACK}
      ADD     ESP,8
{$ENDIF}
      MOV     EDX,EDI     // save sign in EDX
      MOV     EDI,[ESP+MAXDIGS+4] // load result string pointer

      MOV     [EDI],BL    // length of result string := width
      INC     EDI

      MOV     AL,' '      // prepare for leading blanks and sign

      MOV     ECX,EBX     // blankCnt := width - digCnt - 8
      SUB     ECX,ESI
      SUB     ECX,8
      JLE     @@4

      REP     STOSB       // emit blankCnt blanks
@@4:
      SUB     [EDI-1],CL  // if blankCnt < 0, adjust length

      TEST    DL,DL       // emit the sign (' ' or '-')
      JE      @@5
      MOV     AL,'-'
@@5:
      STOSB

      POP     EAX
      MOV     ECX,ESI     // emit digCnt digits
      MOV     ESI,ESP     // point ESI to digBuf

      CMP     byte ptr [ESI],'0'
      JE      @@5a        // if rounding overflowed, adjust exponent and ESI
      INC     EAX
      DEC     ESI
@@5a:
      INC     ESI

      MOVSB                       // emit one digit
      MOV     byte ptr [EDI],'.'  // emit dot
      INC     EDI                 // adjust dest pointer
      DEC     ECX                 // adjust count

      REP     MOVSB

      MOV     byte ptr [EDI],'E'

      MOV     CL,'+'              // emit sign of exponent ('+' or '-')
      TEST    EAX,EAX
      JGE     @@6
      MOV     CL,'-'
      NEG     EAX
@@6:
      MOV     [EDI+1],CL

      XOR     EDX,EDX             // emit exponent
      MOV     CX,10
      DIV     CX
      ADD     DL,'0'
      MOV     [EDI+5],DL

      XOR     EDX,EDX
      DIV     CX
      ADD     DL,'0'
      MOV     [EDI+4],DL

      XOR     EDX,EDX
      DIV     CX
      ADD     DL,'0'
      MOV     [EDI+3],DL

      ADD     AL,'0'
      MOV     [EDI+2],AL

      JMP     @@exit

@@fixed:

//  FST(0)  = value >= 0.0
//  EBX = width
//  ESI = precision
//  EDI = sign

      CMP     ESI,MAXDIGS-40    // else if precision > MAXDIGS-40 then precision := MAXDIGS-40;
      JLE     @@6a
      MOV     ESI,MAXDIGS-40
@@6a:
{$IFDEF PIC}
      PUSH    EAX
      CALL    GetGOT
      FCOM    qword ptr [EAX] + offset Ten
      POP     EAX
{$ELSE !PIC}
      FCOM    qword ptr ten
{$ENDIF !PIC}
      FSTSW   AX
      SAHF
      MOV     EAX,0
      JB      @@7

{$IFDEF ALIGN_STACK}
      SUB     ESP,12
{$ENDIF ALIGN_STACK}
      CALL    _ScaleExt   // val is FST(0), exp is EAX
{$IFDEF ALIGN_STACK}
      ADD     ESP,12
{$ENDIF ALIGN_STACK}
      CMP     EAX,35      // if val is too large, use scientific
      JG      @@scientific

@@7:
//  FST(0)  = scaled value, 0.0 <= value < 10.0
//  EAX = exponent, 0 <= exponent

//  intDigCnt := exponent + 1;

      INC     EAX

//  _EmitDigits( value, intDigCnt + precision, digBuf );

      MOV     EDX,ESP
      PUSH    EAX
      ADD     EAX,ESI
{$IFDEF ALIGN_STACK}
      SUB     ESP,8
{$ENDIF ALIGN_STACK}
      CALL    _EmitDigits
{$IFDEF ALIGN_STACK}
      ADD     ESP,8
{$ENDIF ALIGN_STACK}
      POP     EAX

//  Now we need to check whether rounding to the right number of
//  digits overflowed, and if so, adjust things accordingly

      MOV     EDX,ESI     // put precision in EDX
      MOV     ESI,ESP     // point EDI to digBuf
      CMP     byte ptr [ESI],'0'
      JE      @@8
      INC     EAX
      DEC     ESI
@@8:
      INC     ESI

      MOV     ECX,EAX     // numWidth := sign + intDigCnt;
      ADD     ECX,EDI

      TEST    EDX,EDX     // if precision > 0 then
      JE      @@9
      INC     ECX         //   numWidth := numWidth + 1 + precision
      ADD     ECX,EDX

      CMP     EBX,ECX     // if width <= numWidth
      JG      @@9
      MOV     EBX,ECX     //   width := numWidth
@@9:
      PUSH    EAX
      PUSH    EDI

      MOV     EDI,[ESP+MAXDIGS+2*4] // point EDI to dest string

      MOV     [EDI],BL    // store final length in dest string
      INC     EDI

      SUB     EBX,ECX     // width := width - numWidth
      MOV     ECX,EBX
      JLE     @@10

      MOV     AL,' '      // emit width blanks
      REP     STOSB
@@10:
      SUB     [EDI-1],CL  // if blankCnt < 0, adjust length
      POP     EAX
      POP     ECX

      TEST    EAX,EAX
      JE      @@11

      MOV     byte ptr [EDI],'-'
      INC     EDI

@@11:
      REP     MOVSB       // copy intDigCnt digits

      TEST    EDX,EDX     // if precision > 0 then
      JE      @@12

      MOV     byte ptr [EDI],'.'  //   emit '.'
      INC     EDI
      MOV     ECX,EDX     //   emit precision digits
      REP     MOVSB

@@12:

@@exit:
      ADD     ESP,MAXDIGS
      POP     ECX
      POP     EDI
      POP     ESI
      POP     EBX
      RET     12

@@nanInf:
//  here: EBX = width, ECX = string pointer, EDI = sign, [ESP] = value

{$IFDEF PIC}
      PUSH    ECX
      CALL    GetGOT
      POP     ECX
{$ELSE}
      XOR     EAX,EAX
{$ENDIF}
      FSTP    ST(0)
      CMP     dword ptr [ESP+4],80000000H
      LEA     ESI,[EAX] + offset nanStr
      JNE     @@13
      DEC     EDI
      LEA     ESI,[EAX] + offset plusInfStr
      JNZ     @@13
      LEA     ESI,[EAX] + offset minInfStr
@@13:
      MOV     EDI,ECX
      MOV     ECX,EBX
      MOV     [EDI],CL
      INC     EDI
      SUB     CL,[ESI]
      JBE     @@14
      MOV     AL,' '
      REP     STOSB
@@14:
      SUB     [EDI-1],CL
      MOV     CL,[ESI]
      INC     ESI
      REP     MOVSB

      JMP     @@exit
end;
{$IFEND}




{$IF not defined(CPU386)}
function _Str0Ext(val: Extended): ShortString;
begin
  Result := _Str2Ext(val, 23, -1);
end;
{$ELSE}
procedure _Str0Ext;
asm
// -> [ESP+4] Extended value
//  EAX Pointer to string

      MOV     ECX,EAX     // pass string
      MOV     EAX,23      // pass default field width
      OR      EDX,-1      // pass precision -1
      JMP     _Str2Ext
end;
{$IFEND}

{$IF not defined(CPU386)}

function _Str1Ext(val: Extended; width: LongInt): ShortString;
begin
  Result := _Str2Ext(val, width, -1);
end;
{$ELSE}


procedure _Str1Ext;//( val: Extended; width: Longint; var s: String );
asm
// -> [ESP+4] Extended value
//  EAX Field width
//  EDX Pointer to string

      MOV     ECX,EDX
      OR      EDX,-1      // pass precision -1
      JMP     _Str2Ext
end;
{$IFEND}





{$IF not defined(CPU386)}
function _ValExt(s: string; var code: Integer): Extended;
begin
  code := 0;
  Result := 999;
end;
{$ELSE}
procedure _ValExt;
asm
// -> EAX Pointer to string
//  EDX Pointer to code result
// <- FST(0)  Result

      PUSH    EBX
{$IFDEF PIC}
      PUSH    EAX
      CALL    GetGOT
      MOV     EBX,EAX
      POP     EAX
{$ELSE}
      XOR     EBX,EBX
{$ENDIF}
      PUSH    ESI
      PUSH    EDI

      PUSH    EBX     // SaveGOT = ESP+8
      MOV     ESI,EAX
      PUSH    EAX     // save for the error case

      FLDZ
      XOR     EAX,EAX
      XOR     EBX,EBX
      XOR     EDI,EDI

      PUSH    EBX     // temp to get digs to fpu

      TEST    ESI,ESI
      JE      @@empty

@@blankLoop:
      MOV     BX,[ESI]
      ADD     ESI, 2
      CMP     BX,' '
      JE      @@blankLoop

@@endBlanks:
      MOV     CH,0
      CMP     BX,'-'
      JE      @@minus
      CMP     BX,'+'
      JE      @@plus
      JMP     @@firstDigit

@@minus:
      INC     CH
@@plus:
      MOV     BX,[ESI]
      ADD     ESI, 2

@@firstDigit:
      TEST    BX,BX
      JE      @@error

      MOV     EDI,[ESP+8]     // SaveGOT

@@digLoop:
      SUB     BX,'0'
      CMP     BX,9
      JA      @@dotExp
      FMUL    qword ptr [EDI] + offset Ten
      MOV     dword ptr [ESP],EBX
      FIADD   dword ptr [ESP]
      MOV     BX,[ESI]
      ADD     ESI, 2
      TEST    BX,BX
      JNE     @@digLoop
      JMP     @@prefinish

@@dotExp:
      CMP     BX,'.' - '0'
      JNE     @@exp
      MOV     BX,[ESI]
      ADD     ESI, 2
      TEST    BX,BX
      JE      @@prefinish

//  EDI = SaveGot
@@fracDigLoop:
      SUB     BX,'0'
      CMP     BX,9
      JA      @@exp
      FMUL    qword ptr [EDI] + offset Ten
      MOV     dword ptr [ESP],EBX
      FIADD   dword ptr [ESP]
      DEC     EAX
      MOV     BX,[ESI]
      ADD     ESI, 2
      TEST    BX,BX
      JNE     @@fracDigLoop

@@prefinish:
      XOR     EDI,EDI
      JMP     @@finish

@@exp:
      CMP     BX,'E' - '0'
      JE      @@foundExp
      CMP     BX,'e' - '0'
      JNE     @@error
@@foundExp:
      MOV     BX,[ESI]
      ADD     ESI, 2
      MOV     AH,0
      CMP     BX,'-'
      JE      @@minusExp
      CMP     BX,'+'
      JE      @@plusExp
      JMP     @@firstExpDigit
@@minusExp:
      INC     AH
@@plusExp:
      MOV     BX,[ESI]
      ADD     ESI, 2
@@firstExpDigit:
      SUB     BX,'0'
      CMP     BX,9
      JA      @@error
      MOV     EDI,EBX
      MOV     BX,[ESI]
      ADD     ESI, 2
      TEST    BX,BX
      JZ      @@endExp
@@expDigLoop:
      SUB     BX,'0'
      CMP     BX,9
      JA      @@error
      LEA     EDI,[EDI+EDI*4]
      ADD     EDI,EDI
      ADD     EDI,EBX
      MOV     BX,[ESI]
      ADD     ESI, 2
      TEST    BX,BX
      JNZ     @@expDigLoop
@@endExp:
      DEC     AH
      JNZ     @@expPositive
      NEG     EDI
@@expPositive:
      MOVSX   EAX,AL

@@finish:
      ADD     EAX,EDI
      PUSH    EDX
      PUSH    ECX
      CALL    _Pow10
      POP     ECX
      POP     EDX

      DEC     CH
      JE      @@negate

@@successExit:
      ADD     ESP,12   // pop temp and saved copy of string pointer

      XOR     ESI,ESI   // signal no error to caller

@@exit:
      SHR     ESI,1
      MOV     [EDX],ESI
      POP     EDI
      POP     ESI
      POP     EBX
      RET

@@negate:
      FCHS
      JMP     @@successExit

@@empty:
      ADD     ESI,2

@@error:
      POP     EAX
      POP     EBX
      SUB     ESI,EBX
      ADD     ESP,4
      JMP     @@exit
end;
{$IFEND}




{$IF not defined(CPU386)}
function FPower10(val: Extended; power: Integer): Extended;
begin
  Result := _Pow10(val, power);
end;
{$ELSE}
procedure FPower10;
asm
  JMP  _Pow10
end;
{$IFEND}





{$IF not defined(CPU386)}
function _Pow10(val: Extended; Power: Integer): Extended;
begin
  Result := val;
end;
{$ELSE}
//function _Pow10(val: Extended; Power: Integer): Extended;
procedure _Pow10;
asm
// -> FST(0)  val
// -> EAX Power
// <- FST(0)  val * 10**Power

//  This routine generates 10**power with no more than two
//  floating point multiplications. Up to 10**31, no multiplications
//  are needed.

      PUSH   EBX
{$IFDEF PIC}
      PUSH   EAX
      CALL   GetGOT
      MOV    EBX, EAX
      POP    EAX
{$ELSE}
      XOR    EBX, EBX
{$ENDIF}
      TEST   EAX, EAX
      JL     @@neg
      JE     @@exit
      CMP    EAX, 5120
      JGE    @@inf
      MOV    EDX, EAX
      AND    EDX, 01FH
      LEA    EDX, [EDX+EDX*4]
      FLD    tbyte ptr @@tab0[EBX+EDX*2]

      FMULP

      SHR    EAX,5
      JE     @@exit

      MOV    EDX, EAX
      AND    EDX, 0FH
      JE     @@skip2ndMul
      LEA    EDX, [EDX+EDX*4]
      FLD    tbyte ptr @@tab1-10[EBX+EDX*2]
      FMULP

@@skip2ndMul:
      SHR    EAX, 4
      JE     @@exit
      LEA    EAX, [EAX+EAX*4]
      FLD    tbyte ptr @@tab2-10[EBX+EAX*2]
      FMULP
      JMP    @@exit

@@neg:
      NEG    EAX
      CMP    EAX, 5120
      JGE    @@zero
      MOV    EDX, EAX
      AND    EDX, 01FH
      LEA    EDX, [EDX+EDX*4]
      FLD    tbyte ptr @@tab0[EBX+EDX*2]
      FDIVP

      SHR    EAX, 5
      JE     @@exit

      MOV    EDX, EAX
      AND    EDX, 0FH
      JE     @@skip2ndDiv
      LEA    EDX, [EDX+EDX*4]
      FLD    tbyte ptr @@tab1-10[EBX+EDX*2]
      FDIVP

@@skip2ndDiv:
      SHR    EAX, 4
      JE     @@exit
      LEA    EAX, [EAX+EAX*4]
      FLD    tbyte ptr @@tab2-10[EBX+EAX*2]
      FDIVP

      JMP    @@exit

@@inf:
      FSTP   ST(0)
      FLD    tbyte ptr @@infval[EBX]
      JMP    @@exit

@@zero:
      FSTP   ST(0)
      FLDZ

@@exit:
      POP    EBX
      RET

@@infval:  DW  $0000,$0000,$0000,$8000,$7FFF
@@tab0:    DW  $0000,$0000,$0000,$8000,$3FFF  // 10**0
           DW  $0000,$0000,$0000,$A000,$4002    // 10**1
           DW  $0000,$0000,$0000,$C800,$4005    // 10**2
           DW  $0000,$0000,$0000,$FA00,$4008        // 10**3
           DW  $0000,$0000,$0000,$9C40,$400C        // 10**4
           DW  $0000,$0000,$0000,$C350,$400F        // 10**5
           DW  $0000,$0000,$0000,$F424,$4012        // 10**6
           DW  $0000,$0000,$8000,$9896,$4016        // 10**7
           DW  $0000,$0000,$2000,$BEBC,$4019        // 10**8
           DW  $0000,$0000,$2800,$EE6B,$401C        // 10**9
           DW  $0000,$0000,$F900,$9502,$4020        // 10**10
           DW  $0000,$0000,$B740,$BA43,$4023        // 10**11
           DW  $0000,$0000,$A510,$E8D4,$4026        // 10**12
           DW  $0000,$0000,$E72A,$9184,$402A        // 10**13
           DW  $0000,$8000,$20F4,$B5E6,$402D        // 10**14
           DW  $0000,$A000,$A931,$E35F,$4030        // 10**15
           DW  $0000,$0400,$C9BF,$8E1B,$4034        // 10**16
           DW  $0000,$C500,$BC2E,$B1A2,$4037        // 10**17
           DW  $0000,$7640,$6B3A,$DE0B,$403A        // 10**18
           DW  $0000,$89E8,$2304,$8AC7,$403E        // 10**19
           DW  $0000,$AC62,$EBC5,$AD78,$4041        // 10**20
           DW  $8000,$177A,$26B7,$D8D7,$4044        // 10**21
           DW  $9000,$6EAC,$7832,$8786,$4048        // 10**22
           DW  $B400,$0A57,$163F,$A968,$404B        // 10**23
           DW  $A100,$CCED,$1BCE,$D3C2,$404E        // 10**24
           DW  $84A0,$4014,$5161,$8459,$4052        // 10**25
           DW  $A5C8,$9019,$A5B9,$A56F,$4055        // 10**26
           DW  $0F3A,$F420,$8F27,$CECB,$4058        // 10**27
           DW  $0984,$F894,$3978,$813F,$405C        // 10**28
           DW  $0BE5,$36B9,$07D7,$A18F,$405F        // 10**29
           DW  $4EDF,$0467,$C9CD,$C9F2,$4062        // 10**30
           DW  $2296,$4581,$7C40,$FC6F,$4065        // 10**31

@@tab1:    DW  $B59E,$2B70,$ADA8,$9DC5,$4069        // 10**32
           DW  $A6D5,$FFCF,$1F49,$C278,$40D3        // 10**64
           DW  $14A3,$C59B,$AB16,$EFB3,$413D        // 10**96
           DW  $8CE0,$80E9,$47C9,$93BA,$41A8        // 10**128
           DW  $17AA,$7FE6,$A12B,$B616,$4212        // 10**160
           DW  $556B,$3927,$F78D,$E070,$427C        // 10**192
           DW  $C930,$E33C,$96FF,$8A52,$42E7        // 10**224
           DW  $DE8E,$9DF9,$EBFB,$AA7E,$4351        // 10**256
           DW  $2F8C,$5C6A,$FC19,$D226,$43BB        // 10**288
           DW  $E376,$F2CC,$2F29,$8184,$4426        // 10**320
           DW  $0AD2,$DB90,$2700,$9FA4,$4490        // 10**352
           DW  $AA17,$AEF8,$E310,$C4C5,$44FA        // 10**384
           DW  $9C59,$E9B0,$9C07,$F28A,$4564        // 10**416
           DW  $F3D4,$EBF7,$4AE1,$957A,$45CF        // 10**448
           DW  $A262,$0795,$D8DC,$B83E,$4639        // 10**480

@@tab2:    DW  $91C7,$A60E,$A0AE,$E319,$46A3        // 10**512
           DW  $0C17,$8175,$7586,$C976,$4D48        // 10**1024
           DW  $A7E4,$3993,$353B,$B2B8,$53ED        // 10**1536
           DW  $5DE5,$C53D,$3B5D,$9E8B,$5A92        // 10**2048
           DW  $F0A6,$20A1,$54C0,$8CA5,$6137        // 10**2560
           DW  $5A8B,$D88B,$5D25,$F989,$67DB        // 10**3072
           DW  $F3F8,$BF27,$C8A2,$DD5D,$6E80        // 10**3584
           DW  $979B,$8A20,$5202,$C460,$7525        // 10**4096
           DW  $59F0,$6ED5,$1162,$AE35,$7BCA        // 10**4608
end;
{$IFEND}

const
  RealBias = 129;
  ExtBias  = $3FFF;




{$IF not defined(CPU386)}
function _Real2Ext(val: Pointer {PReal48}): Extended;
begin
  Result := 0;
end;
{$ELSE}
procedure _Real2Ext;//( val : Real ) : Extended;
asm
// -> EAX Pointer to value
// <- FST(0)  Result

//  the REAL data type has the following format:
//  8 bit exponent (bias 129), 39 bit fraction, 1 bit sign

        MOV    DH, [EAX+5]  // isolate the sign bit
        AND    DH, 80H
        MOV    DL, [EAX]  // fetch exponent
        TEST   DL, DL   // exponent zero means number is zero
        JE     @@zero

        ADD    DX, ExtBias - RealBias // adjust exponent bias

        PUSH   EDX   // the exponent is at the highest address

        MOV    EDX, [EAX+2] // load high fraction part, set hidden bit
        OR     EDX, 80000000H
        PUSH   EDX   // push high fraction part

        MOV    DL, [EAX+1]  // load remaining low byte of fraction
        SHL    EDX, 24    // clear low 24 bits
        PUSH   EDX

        FLD    tbyte ptr [ESP] // pop result onto chip
        ADD    ESP, 12

        RET

@@zero:
        FLDZ
        RET
end;
{$IFEND}




{$IF not defined(CPU386)}
function _Ext2Real(val: Extended) : Real48;
begin
end;
{$ELSE}
procedure _Ext2Real;//( val : Extended ) : Real;
asm
// -> FST(0)  Value
//  EAX Pointer to result

        PUSH  EBX

        SUB   ESP,12
        FSTP  tbyte ptr [ESP]

        POP   EBX     // EBX is low half of fraction
        POP   EDX     // EDX is high half of fraction
        POP   ECX     // CX is exponent and sign

        SHR   EBX,24  // set carry to last bit shifted out
        ADC   BL,0    // if bit was 1, round up
        ADC   EDX,0
        ADC   CX,0
        JO    @@overflow

        ADD   EDX,EDX // shift fraction 1 bit left
        ADD   CX,CX   // shift sign bit into carry
        RCR   EDX,1   // attach sign bit to fraction
        SHR   CX,1    // restore exponent, deleting sign

        SUB   CX,ExtBias-RealBias // adjust exponent
        JLE   @@underflow
        TEST  CH,CH     // CX must be in 1..255
        JG    @@overflow

        MOV   [EAX],CL
        MOV   [EAX+1],BL
        MOV   [EAX+2],EDX

        POP   EBX
        RET

@@underflow:
        XOR   ECX,ECX
        MOV   [EAX],ECX
        MOV   [EAX+4],CX
        POP   EBX
        RET

@@overflow:
        POP   EBX
        MOV   AL,8
        JMP   Error
end;
{$IFEND}

const
    ovtInstanceSize = -8;   { Offset of instance size in OBJECTs    }
    ovtVmtPtrOffs   = -4;



{$IF not defined(CPU386)}
procedure _ObjSetup;
begin
end;
{$ELSE}
procedure       _ObjSetup;
asm //StackAlignSafe
{       FUNCTION _ObjSetup( self: ^OBJECT; vmt: ^VMT): ^OBJECT; }
{     ->EAX     Pointer to self (possibly nil)  }
{       EDX     Pointer to vmt  (possibly nil)  }
{     <-EAX     Pointer to self                 }
{       EDX     <> 0: an object was allocated   }
{       Z-Flag  Set: failure, Cleared: Success  }

        CMP     EDX,1   { is vmt = 0, indicating a call         }
        JAE     @@skip1 { from a constructor?                   }
        RET                     { return immediately with Z-flag cleared        }

@@skip1:
        PUSH    ECX
        TEST    EAX,EAX { is self already allocated?            }
        JNE     @@noAlloc
        MOV     EAX,[EDX].ovtInstanceSize
        TEST    EAX,EAX
        JE      @@zeroSize
        PUSH    EDX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    _GetMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        POP     EDX
        TEST    EAX,EAX
        JZ      @@fail

        {       Zero fill the memory }
        PUSH    EDI
        MOV     ECX,[EDX].ovtInstanceSize
        MOV     EDI,EAX
        PUSH    EAX
        XOR     EAX,EAX
        SHR     ECX,2
        REP     STOSD
        MOV     ECX,[EDX].ovtInstanceSize
        AND     ECX,3
        REP     STOSB
        POP     EAX
        POP     EDI

        MOV     ECX,[EDX].ovtVmtPtrOffs
        TEST    ECX,ECX
        JL      @@skip
        MOV     [EAX+ECX],EDX   { store vmt in object at this offset    }
@@skip:
        TEST    EAX,EAX { clear zero flag                               }
        POP     ECX
        RET

@@fail:
        XOR     EDX,EDX
        POP     ECX
        RET

@@zeroSize:
        XOR     EDX,EDX
        CMP     EAX,1   { clear zero flag - we were successful (kind of) }
        POP     ECX
        RET

@@noAlloc:
        MOV     ECX,[EDX].ovtVmtPtrOffs
        TEST    ECX,ECX
        JL      @@exit
        MOV     [EAX+ECX],EDX   { store vmt in object at this offset    }
@@exit:
        XOR     EDX,EDX { clear allocated flag                  }
        TEST    EAX,EAX { clear zero flag                               }
        POP     ECX
end;
{$IFEND}




procedure _ObjCopy;
{$IF not defined(CPU386)}
begin
end;
{$ELSE}
asm
{       PROCEDURE _ObjCopy( dest, src: ^OBJECT; vmtPtrOff: Longint);    }
{     ->EAX     Pointer to destination          }
{       EDX     Pointer to source               }
{       ECX     Offset of vmt in those objects. }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EDX
        MOV     EDI,EAX

        LEA     EAX,[EDI+ECX]   { remember pointer to dest vmt pointer  }
        MOV     EDX,[EAX]       { fetch dest vmt pointer        }

        MOV     EBX,[EDX].ovtInstanceSize

        MOV     ECX,EBX { copy size DIV 4 dwords        }
        SHR     ECX,2
        REP     MOVSD

        MOV     ECX,EBX { copy size MOD 4 bytes }
        AND     ECX,3
        REP     MOVSB

        MOV     [EAX],EDX       { restore dest vmt              }

        POP     EDI
        POP     ESI
        POP     EBX
end;
{$IFEND}




function _Fail(self: Pointer; allocFlag: LongInt): Pointer;
{       FUNCTION _Fail( self: ^OBJECT; allocFlag:Longint): ^OBJECT;     }
{     ->EAX     Pointer to self (possibly nil)          }
{       EDX     <> 0: Object must be deallocated        }
{     <-EAX     Nil                                     }
begin
  if allocFlag <> 0 then
    FreeMem(self);
  Result := nil;
end;





{$IF not defined(CPU386)}
procedure _FpuInit;
begin
end;
{$ELSE}
procedure       _FpuInit;
asm
        FNINIT
        FWAIT
{$IFDEF PIC}
        CALL    GetGOT
        MOV     EAX,[EAX].OFFSET Default8087CW
        FLDCW   [EAX]
{$ELSE}
        FLDCW   Default8087CW
{$ENDIF}
end;
{$IFEND}




procedure _BoundErr;
{$IF not defined(CPU386)}
begin
  Error(reRangeError);
end;
{$ELSE}
asm
        MOV     AL,reRangeError
        JMP     Error
end;
{$IFEND}




procedure _IntOver;
{$IF not defined(CPU386)}
begin
  Error(reIntOverflow);
end;
{$ELSE}
asm
        MOV     AL,reIntOverflow
        JMP     Error
end;
{$IFEND}

{$IFDEF POSIX}
function InternalCompareText(const S1, S2: string): Boolean;
var
  I: Integer;
  US1, US2: UCS4String;
  LCompareLocale: Pointer;
begin
  if Length(S1) <> Length(S2) then
    Exit(False);

  // Convert to UCS4
  US1 := UnicodeStringToUCS4String(S1);
  US2 := UnicodeStringToUCS4String(S2);

  // Convert to upper case for case insensitivity
  LCompareLocale := UTF8CompareLocale;
  for I := 0 to Length(US1) - 1 do
    US1[I] := UCS4Char(towupper_l(wint_t(US1[I]), LCompareLocale));
  for I := 0 to Length(US2) - 1 do
    US2[I] := UCS4Char(towupper_l(wint_t(US2[I]), LCompareLocale));

  // Clear error info and compare strings
  SetLastError(0);
  Result := (wcscoll_l(pwchar_t(@US1[0]), pwchar_t(@US2[0]), LCompareLocale) = 0) and
            (GetLastError = 0);
end;
{$ENDIF}

function TObject.ClassType: TClass;
begin
  Pointer(Result) := PPointer(Self)^;
end;

class function TObject.ClassName: string;
begin
  Result := UTF8ToString(PShortString(PPointer(NativeInt(Self) + vmtClassName)^)^);
end;



class function TObject.ClassNameIs(const Name: string): Boolean;
{$IFDEF MSWINDOWS}
var
  LClassName: string;
begin
  LClassName := ClassName;
  Result := CompareString(UTF8CompareLocale, NORM_IGNORECASE, PChar(LClassName),
    Length(LClassName), PChar(Name), Length(Name)) = CSTR_EQUAL;
end;
{$ENDIF}
{$IFDEF POSIX}
begin
  Result := InternalCompareText(ClassName, Name);
end;
{$ENDIF}




class function TObject.ClassParent: TClass;
{$IFDEF PUREPASCAL}
begin
  Pointer(Result) := PPointer(NativeInt(Self) + vmtParent)^;
  if Result <> nil then
    Pointer(Result) := PPointer(Result)^;
end;
{$ELSE !PUREPASCAL}
asm
        MOV     EAX,[EAX].vmtParent
        TEST    EAX,EAX
        JE      @@exit
        MOV     EAX,[EAX]
@@exit:
end;
{$ENDIF PUREPASCAL}

class function TObject.NewInstance: TObject;
begin
  Result := InitInstance(_GetMem(InstanceSize));
end;

procedure TObject.FreeInstance;
begin
  CleanupInstance;
  _FreeMem(Self);
end;

class function TObject.InstanceSize: Longint;
begin
  Result := PInteger(NativeInt(Self) + vmtInstanceSize)^;
end;

constructor TObject.Create;
begin
end;

destructor TObject.Destroy;
begin
end;

procedure TObject.Free;
begin
  if Self <> nil then
    Destroy;
end;




class function TObject.InitInstance(Instance: Pointer): TObject;
{$IFDEF PUREPASCAL}
var
  IntfTable: PInterfaceTable;
  ClassPtr: TClass;
  I: Integer;
begin
  FillChar(Instance^, InstanceSize, 0);
  PPointer(Instance)^ := Pointer(Self);
  ClassPtr := Self;
  while ClassPtr <> nil do
  begin
    IntfTable := ClassPtr.GetInterfaceTable;
    if IntfTable <> nil then
      for I := 0 to IntfTable.EntryCount-1 do
        with IntfTable.Entries[I] do
        begin
          if VTable <> nil then
            PPointer(@PByte(Instance)[IOffset])^ := VTable;
        end;
    ClassPtr := ClassPtr.ClassParent;
  end;
  Result := Instance;
end;
{$ELSE !PUREPASCAL}
asm
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     EDI,EDX
        STOSD
        MOV     ECX,[EBX].vmtInstanceSize
        XOR     EAX,EAX
        PUSH    ECX
        SHR     ECX,2
        DEC     ECX
        REP     STOSD
        POP     ECX
        AND     ECX,3
        REP     STOSB
        MOV     EAX,EDX
        MOV     EDX,ESP
@@0:    MOV     ECX,[EBX].vmtIntfTable
        TEST    ECX,ECX
        JE      @@1
        PUSH    ECX
@@1:    MOV     EBX,[EBX].vmtParent
        TEST    EBX,EBX
        JE      @@2
        MOV     EBX,[EBX]
        JMP     @@0
@@2:    CMP     ESP,EDX
        JE      @@5
@@3:    POP     EBX
        MOV     ECX,[EBX].TInterfaceTable.EntryCount
        ADD     EBX,4
@@4:    MOV     ESI,[EBX].TInterfaceEntry.VTable
        TEST    ESI,ESI
        JE      @@4a
        MOV     EDI,[EBX].TInterfaceEntry.IOffset
        MOV     [EAX+EDI],ESI
@@4a:   ADD     EBX,TYPE TInterfaceEntry
        DEC     ECX
        JNE     @@4
        CMP     ESP,EDX
        JNE     @@3
@@5:    POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF}




procedure TObject.CleanupInstance;
{$IFDEF PUREPASCAL}
var
  ClassPtr: TClass;
  InitTable: Pointer;
begin
  ClassPtr := ClassType;
  repeat
    InitTable := PPointer(NativeInt(ClassPtr) + vmtInitTable)^;
    if InitTable <> nil then
      _FinalizeRecord(Self, InitTable);

    ClassPtr := ClassPtr.ClassParent;
  until ClassPtr = nil;
  TMonitor.Destroy(Self);
end;
{$ELSE !PUREPASCAL}
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
        MOV     ESI,EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
@@loop:
        MOV     ESI,[ESI]
        MOV     EDX,[ESI].vmtInitTable
        MOV     ESI,[ESI].vmtParent
        TEST    EDX,EDX
        JE      @@skip
        CALL    _FinalizeRecord
        MOV     EAX,EBX
@@skip:
        TEST    ESI,ESI
        JNE     @@loop

        MOV     EAX,EBX
        CALL    TMonitor.Destroy;
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}

        POP     ESI
        POP     EBX
end;
{$ENDIF}




function InvokeImplGetter(Self: TObject; ImplGetter: NativeUInt): IInterface;
{$IFDEF PUREPASCAL}
var
  M: function: IInterface of object;
begin
  TMethod(M).Data := Self;
  case LongWord(ImplGetter) of
    $FF000000..$FFFFFFFF:  // Field
        Result := IInterface(Pointer(NativeUInt(Self) + (ImplGetter and $00FFFFFF)));
    $FE000000..$FEFFFFFF:  // virtual method
      begin
        // sign extend vmt slot offset = smallint cast
        TMethod(M).Code := PPointer(NativeInt(Self) + SmallInt(ImplGetter))^;
        Result := M;
      end;
  else // static method
    TMethod(M).Code := Pointer(ImplGetter);
    Result := M;
  end;
end;
{$ELSE !PUREPASCAL}
asm
        XCHG    EDX,ECX
        CMP     ECX,$FF000000
        JAE     @@isField
        CMP     ECX,$FE000000
        JB      @@isStaticMethod

        {       the GetProc is a virtual method }
        MOVSX   ECX,CX                  { sign extend slot offs }
        ADD     ECX,[EAX]               { vmt   + slotoffs      }
        JMP     dword ptr [ECX]         { call vmt[slot]        }

@@isStaticMethod:
        JMP     ECX

@@isField:
        AND     ECX,$00FFFFFF
        ADD     ECX,EAX
        MOV     EAX,EDX
        MOV     EDX,[ECX]
        JMP     _IntfCopy
end;
{$ENDIF}

function TObject.Equals(Obj: TObject): Boolean;
begin
  Result := Obj = Self;
end;

function TObject.GetHashCode: Integer;
begin
  {$IF defined(CPU386)}
  Result := Integer(Self);
  {$ELSE}
  Result := Integer(Self) xor Integer(NativeInt(Self) shr 32);
  {$IFEND}
end;

function TObject.GetInterface(const IID: TGUID; out Obj): Boolean;
var
  InterfaceEntry: PInterfaceEntry;
begin
  Pointer(Obj) := nil;
  InterfaceEntry := GetInterfaceEntry(IID);
  if InterfaceEntry <> nil then
  begin
    if InterfaceEntry^.IOffset <> 0 then
    begin
      Pointer(Obj) := Pointer(NativeInt(Self) + InterfaceEntry^.IOffset);
      if Pointer(Obj) <> nil then IInterface(Obj)._AddRef;
    end
    else
      IInterface(Obj) := InvokeImplGetter(Self, InterfaceEntry^.ImplGetter);
  end else if (Int64(ObjCastGUID.D1) = Int64(IID.D1)) and
              (Int64(ObjCastGUID.D4) = Int64(IID.D4)) then
    Pointer(Obj) := Self;
  Result := Pointer(Obj) <> nil;
end;




class function TObject.GetInterfaceEntry(const IID: TGUID): PInterfaceEntry;
{$IFDEF PUREPASCAL}
var
  ClassPtr: TClass;
  IntfTable: PInterfaceTable;
  I: Integer;
begin
  ClassPtr := Self;
  repeat
    IntfTable := ClassPtr.GetInterfaceTable;
    if IntfTable <> nil then
      for I := 0 to IntfTable.EntryCount-1 do
      begin
        Result := @IntfTable.Entries[I];
//        if Result^.IID = IID then Exit;
        if (Int64(Result^.IID.D1) = Int64(IID.D1)) and
           (Int64(Result^.IID.D4) = Int64(IID.D4)) then Exit;
      end;
    ClassPtr := ClassPtr.ClassParent;
  until ClassPtr = nil;
  Result := nil;
end;
{$ELSE}
asm
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
@@1:    MOV     EAX,[EBX].vmtIntfTable
        TEST    EAX,EAX
        JE      @@4
        MOV     ECX,[EAX].TInterfaceTable.EntryCount
        ADD     EAX,4
@@2:    MOV     ESI,[EDX].Integer[0]
        CMP     ESI,[EAX].TInterfaceEntry.IID.Integer[0]
        JNE     @@3
        MOV     ESI,[EDX].Integer[4]
        CMP     ESI,[EAX].TInterfaceEntry.IID.Integer[4]
        JNE     @@3
        MOV     ESI,[EDX].Integer[8]
        CMP     ESI,[EAX].TInterfaceEntry.IID.Integer[8]
        JNE     @@3
        MOV     ESI,[EDX].Integer[12]
        CMP     ESI,[EAX].TInterfaceEntry.IID.Integer[12]
        JE      @@5
@@3:    ADD     EAX,type TInterfaceEntry
        DEC     ECX
        JNE     @@2
@@4:    MOV     EBX,[EBX].vmtParent
        TEST    EBX,EBX
        JE      @@4a
        MOV     EBX,[EBX]
        JMP     @@1
@@4a:   XOR     EAX,EAX
@@5:    POP     ESI
        POP     EBX
end;
{$ENDIF}

class function TObject.GetInterfaceTable: PInterfaceTable;
begin
  Result := PPointer(NativeInt(Self) + vmtIntfTable)^;
end;

type
  PClassData = ^TClassData;
  TClassData = record
    ClassType: TClass;
    ParentInfo: Pointer;
    PropCount: SmallInt;
    UnitName: ShortString;
  end;

class function TObject.UnitName: string;
var
  LClassInfo: Pointer;
begin
  LClassInfo := ClassInfo;
  if LClassInfo <> nil then
    Result := UTF8ToString(PClassData(NativeInt(LClassInfo) + 2 + PByte(NativeInt(LClassInfo) + 1)^).UnitName)
  else
    Result := '';
end;

function _IsClass(Child: TObject; Parent: TClass): Boolean;
begin
  Result := (Child <> nil) and Child.InheritsFrom(Parent);
end;





function _AsClass(Child: TObject; Parent: TClass): TObject;
{$IFDEF PUREPASCAL}
begin
  Result := Child;
  if not (Child is Parent) then
    Error(reInvalidCast);   // loses return address
end;
{$ELSE}
asm
        { ->    EAX     left operand (class)    }
        {       EDX     VMT of right operand    }
        { <-    EAX     if left is derived from right, else runtime error       }
        TEST    EAX,EAX
        JE      @@exit
        MOV     ECX,EAX
@@loop:
        MOV     ECX,[ECX]
        CMP     ECX,EDX
        JE      @@exit
        MOV     ECX,[ECX].vmtParent
        TEST    ECX,ECX
        JNE     @@loop

        {       do runtime error        }
        MOV     AL,reInvalidCast
        JMP     Error

@@exit:
end;
{$ENDIF}




function _IntfAsClass(const Intf: IInterface; Parent: TClass): TObject;
{$IF not defined(CPU386)}
var
  Temp: IInterface;
begin
  _IntfCast(Temp, Intf, ObjCastGUID);
  Result := _AsClass(TObject(Pointer(Temp)), Parent);
end;
{$ELSE}
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EDX
        PUSH    0
        MOV     EDX, EAX
        LEA     ECX, ObjCastGUID
        MOV     EAX, ESP
        CALL    _IntfCast
        POP     EAX
        POP     EDX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        JMP    _AsClass
end;
{$IFEND}

function _SafeIntfAsClass(const Intf: IInterface; Parent: TClass): TObject;
begin
  if (Intf <> nil) and (Intf.QueryInterface(ObjCastGUID, Result) = S_OK) and (Result is Parent) then
    Exit;
  Result := nil;
end;

function _IntfIsClass(const Intf: IInterface; Parent: TClass): Boolean;
begin
  Result := _SafeIntfAsClass(Intf, Parent) <> nil;
end;

function _GetHelperDelegate(Instance: TObject; HelperClass: TClass): TObject;
begin
  Result := TClassHelperBaseClass(HelperClass)._Create(Instance);
end;

function _GetHelperIntf(Instance: TObject; HelperClass: TClass): IInterface;
var
  IntfTable: PInterfaceTable;
  P: PInterfaceEntry;
begin
  IntfTable := HelperClass.GetInterfaceTable;
  if IntfTable <> nil then
  begin
    if IntfTable.EntryCount > 0 then
    begin
      P := @IntfTable.Entries[0];
      if Instance.GetInterfaceEntry(P.IID) <> nil then
      begin
        Result := TClassHelperBase(Instance);
        Exit;
      end;
    end;
  end;
  Result := TClassHelperBase(_GetHelperDelegate(Instance, HelperClass));
end;

{$IF not defined(CPU386)}

function FindDynaMethod(vmt: Pointer{TClass}; selector: Smallint): Pointer;
type
  TDynaMethodTable = record
    Count: Word;
    Selectors: array[0..9999999] of SmallInt;
    {Addrs: array[0..0] of Pointer;}
  end;
  PDynaMethodTable = ^TDynaMethodTable;
var
  dynaTab: PDynaMethodTable;
  Parent: Pointer;
  Addrs: PPointer;
  I: Cardinal;
begin
  while True do
  begin
    dynaTab := PPointer(NativeInt(vmt) + vmtDynamicTable)^;
    if dynaTab <> nil then
    begin
      for I := 0 to dynaTab.Count do
        if dynaTab.Selectors[I] = selector then
        begin
          Addrs := PPointer(NativeUInt(@dynaTab.Selectors) + I * dynaTab.Count * SizeOf(dynaTab.Selectors[0]));
          Result := PPointer(NativeUInt(Addrs) + I * SizeOf(Pointer))^;
          Exit;
        end;
    end;
    Parent := PPointer(NativeInt(vmt) + vmtParent)^;
    if Parent = nil then Break;
    vmt := PPointer(Parent)^;
  end;
  Result := nil;
end;
{$ELSE}


// FindDynaMethod is an internal function that is safe to call unaligned and
// will never call another function.
procedure FindDynaMethod;
{       function        FindDynaMethod(vmt: TClass; selector: Smallint) : Pointer;       }
asm
        { ->    EAX     vmt of class            }
        {       ESI     dynamic method index    }
        { <-    ESI     pointer to routine      }
        {       ZF = 0 if found                 }
        {       trashes: EAX, ECX               }

        PUSH    EDI
        XCHG    EAX,ESI
        JMP     @@haveVMT
@@outerLoop:
        MOV     ESI,[ESI]
@@haveVMT:
        MOV     EDI,[ESI].vmtDynamicTable
        TEST    EDI,EDI
        JE      @@parent
        MOVZX   ECX,word ptr [EDI]
        PUSH    ECX
        ADD     EDI,2
        REPNE   SCASW
        JE      @@found
        POP     ECX
@@parent:
        MOV     ESI,[ESI].vmtParent
        TEST    ESI,ESI
        JNE     @@outerLoop
        JMP     @@exit

@@found:
        POP     EAX
        ADD     EAX,EAX
        SUB     EAX,ECX         { this will always clear the Z-flag ! }
        MOV     ESI,[EDI+EAX*2-4]

@@exit:
        POP     EDI
end;
{$IFEND}




function GetDynaMethod(vmt: TClass; selector: Smallint): Pointer;
{$IF not defined(CPU386)}
begin
  Result := FindDynaMethod(Pointer(vmt), selector);
end;
{$ELSE}
asm
        PUSH    ESI
        MOV     ESI,EDX
        CALL    FindDynaMethod { Safe to call unaligned }
        JZ      @@notFound
        MOV     EAX,ESI
        JMP     @@exit

@@notFound:
        XOR     EAX,EAX

@@exit:
        POP     ESI
end;
{$IFEND}



{$IF defined(CPU386)}
procedure _CallDynaInst;
asm
        { ->    EAX     vmt of class                 }
        {       ESI     dynamic method index         }
        {       trashes: ESI but compiler knows that }

        PUSH    EAX
        PUSH    ECX
        MOV     EAX,[EAX]
        CALL    FindDynaMethod { Safe to call unaligned }
        POP     ECX
        POP     EAX
        JE      @@Abstract
        JMP     ESI

@@Abstract:
{$IFNDEF ALIGN_STACK}
        POP     ECX
{$ENDIF}
        JMP     _AbstractError
end;
{$IFEND}



{$IF defined(CPU386)}
procedure _CallDynaClass;
asm
        { ->    EAX     vmt of class                 }
        {       ESI     dynamic method index         }
        {       trashes: ESI but compiler knows that }

        PUSH    EAX
        PUSH    ECX
        CALL    FindDynaMethod { Safe to call unaligned }
        POP     ECX
        POP     EAX
        JE      @@Abstract
        JMP     ESI

@@Abstract:
{$IFNDEF ALIGN_STACK}
        POP     ECX
{$ENDIF}
        JMP     _AbstractError
end;
{$IFEND}




{$IF not defined(CPU386)}
function _FindDynaInst(Self: TObject; Selector: SmallInt): Pointer;
begin
  Result := FindDynaMethod(PPointer(Self)^, Selector);
  if Result = nil then
    _AbstractError;
end;
{$ELSE}
procedure _FindDynaInst;
asm
        { ->    EAX     vmt of class                 }
        {       ESI     dynamic method index         }
        { <-    EAX     pointer to method            }

        PUSH    ESI
        MOV     ESI,EDX
        MOV     EAX,[EAX]
        CALL    FindDynaMethod { Safe to call unaligned }
        MOV     EAX,ESI
        POP     ESI
        JNE     @@exit
{$IFNDEF ALIGN_STACK}
        POP     ECX
{$ENDIF}
        JMP     _AbstractError
@@exit:
end;
{$IFEND}




{$IF not defined(CPU386)}
function _FindDynaClass(Vmt: TClass; Selector: SmallInt): Pointer;
begin
  Result := FindDynaMethod(Pointer(Vmt), Selector);
  if Result = nil then
    _AbstractError;
end;
{$ELSE}
procedure       _FindDynaClass;
asm
        { ->    EAX     vmt of class                 }
        {       ESI     dynamic method index         }
        { <-    EAX     pointer to method            }

        PUSH    ESI
        MOV     ESI,EDX
        CALL    FindDynaMethod { Safe to call unaligned }
        MOV     EAX,ESI
        POP     ESI
        JNE     @@exit
{$IFNDEF ALIGN_STACK}
        POP     ECX
{$ENDIF}
        JMP     _AbstractError
@@exit:
end;
{$IFEND}




class function TObject.InheritsFrom(AClass: TClass): Boolean;
{$IFDEF PUREPASCAL}
var
  ClassPtr: Pointer;
  P: Pointer;
begin
  Result := False;
  ClassPtr := Pointer(Self);
  while True do
  begin
    if ClassPtr = Pointer(AClass) then
    begin
      Result := True;
      break;
    end;
    P := PPointer(NativeInt(ClassPtr) + vmtParent)^;
    if P = nil then break;
    ClassPtr := PPointer(P)^;
  end;
end;
{$ELSE}
asm
        { ->    EAX     Pointer to our class    }
        {       EDX     Pointer to AClass       }
        { <-    AL      Boolean result          }
        JMP     @@haveVMT
@@loop:
        MOV     EAX,[EAX]
@@haveVMT:
        CMP     EAX,EDX
        JE      @@success
        MOV     EAX,[EAX].vmtParent
        TEST    EAX,EAX
        JNE     @@loop
        JMP     @@exit
@@success:
        MOV     AL,1
@@exit:
end;
{$ENDIF}


class function TObject.ClassInfo: Pointer;
begin
  Result := PPointer(NativeInt(Self) + vmtTypeInfo)^;
end;

function TObject.SafeCallException(ExceptObject: TObject;
  ExceptAddr: Pointer): HResult;
begin
  Result := HResult($8000FFFF); { E_UNEXPECTED }
end;

function TObject.ToString: string;
begin
  Result := ClassName;
end;

procedure TObject.DefaultHandler(var Message);
begin
end;

procedure TObject.AfterConstruction;
begin
end;

procedure TObject.BeforeDestruction;
begin
end;




procedure TObject.Dispatch(var Message);
{$IF not defined(CPU386)}
type
  //THandlerProc = procedure(Self: Pointer; var Message) { of object };
  THandlerProc = procedure(var Message) of object;
var
  MsgID: Word;
  Addr: Pointer;
  M: THandlerProc;
begin
  MsgID := TDispatchMessage(Message).MsgID;
  if (MsgID <> 0) and (MsgID < $C000) then
  begin
    Addr := FindDynaMethod(PPointer(Self)^, MsgID);
    if Addr <> nil then
    begin
      //THandlerProc(Addr)(Self, Message)
      TMethod(M).Data := Self;
      TMethod(M).Code := Addr;
      M(Message);
    end
    else
      Self.DefaultHandler(Message);
  end
  else
    Self.DefaultHandler(Message);
end;
{$ELSE}
asm
        PUSH    ESI
        MOV     SI,[EDX]
        OR      SI,SI
        JE      @@default
        CMP     SI,0C000H
        JAE     @@default
        PUSH    EAX
{$IFDEF ALIGN_STACK}
        PUSH    EAX
{$ENDIF ALIGN_STACK}
        MOV     EAX,[EAX]
        CALL    FindDynaMethod
{$IFDEF ALIGN_STACK}
        POP     EAX
{$ENDIF ALIGN_STACK}
        POP     EAX
        JE      @@default
        MOV     ECX,ESI
        POP     ESI
        JMP     ECX

@@default:
        POP     ESI
        MOV     ECX,[EAX]
        JMP     DWORD PTR [ECX] + VMTOFFSET TObject.DefaultHandler
end;
{$IFEND}


function UTF8Compare(const Str1, Str2: ShortString): Boolean;
{$IFDEF MSWINDOWS}
var
  Len1, Len2: Integer;
  LStr1, LStr2: array[0..255] of WideChar;
begin
  Len1 := MultiByteToWideChar(CP_UTF8, 0, @Str1[1], Length(Str1), LStr1, Length(LStr1));
  Len2 := MultiByteToWideChar(CP_UTF8, 0, @Str2[1], Length(Str2), LStr2, Length(LStr2));
  Result := CompareStringW(UTF8CompareLocale, NORM_IGNORECASE, LStr1, Len1, LStr2, Len2) = CSTR_EQUAL;
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  Result := InternalCompareText(UTF8ToUnicodeString(Str1), UTF8ToUnicodeString(Str2));
end;
{$ENDIF POSIX}

function UTF8ShortStringToString(const Str: ShortString): string;
begin
  Result := UTF8ToString(Str);
end;




class function TObject.MethodAddress(const Name: ShortString): Pointer;
{$IF not defined(CPU386)}
begin
end;
{$ELSE}
asm //StackAlignSafe
        { ->    EAX     Pointer to class        }
        {       EDX     Pointer to name         }
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        { STACK ALIGN = 16 }
        XOR     ECX,ECX
        XOR     EDI,EDI
        MOV     BL,[EDX]
        JMP     @@haveVMT
@@outer:                                { upper 16 bits of ECX are 0 !  }
        MOV     EAX,[EAX]
@@haveVMT:
        MOV     ESI,[EAX].vmtMethodTable
        TEST    ESI,ESI
        JE      @@parent
        MOV     DI,[ESI]                { EDI := method count           }
        TEST    EDI,EDI
        JZ      @@parent
        ADD     ESI,2
@@inner:                                { upper 16 bits of ECX are 0 !  }
        MOV     CL,[ESI+6]              { compare length of strings     }
        CMP     CL,BL
        JE      @@cmpChar
@@cont:                                 { upper 16 bits of ECX are 0 !  }
        MOV     CX,[ESI]                { fetch length of method desc   }
        ADD     ESI,ECX                 { point ESI to next method      }
        DEC     EDI
        JNZ     @@inner
@@parent:
        MOV     EAX,[EAX].vmtParent     { fetch parent vmt              }
        TEST    EAX,EAX
        JNE     @@outer
        JMP     @@exit                  { return NIL                    }

@@notEqual:
        MOV     BL,[EDX]                { restore BL to length of name  }
        JMP     @@cont

@@utf8Cmp:
        { STACK ALIGN = 16 }
        PUSH    EAX
        PUSH    EDX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF}
        LEA     EAX,[ESI+6]
        CALL    UTF8Compare
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF}
        XOR     ECX,ECX
        TEST    AL,AL
        POP     EDX
        POP     EAX
        JZ      @@notEqual
        JMP     @@foundIt

@@cmpChar:                              { upper 16 bits of ECX are 0 !  }
        MOV     CH,0                    { upper 24 bits of ECX are 0 !  }
@@cmpCharLoop:
        MOV     BL,[ESI+ECX+6]          { case insensitive string cmp   }
        TEST    BL,$80
        JNZ     @@utf8Cmp
        XOR     BL,[EDX+ECX+0]          { last char is compared first   }
        TEST    BL,$80
        JNZ     @@utf8Cmp
        AND     BL,$DF
        JNE     @@notEqual
        DEC     ECX                     { ECX serves as counter         }
        JNZ     @@cmpCharLoop

@@foundIt:
        { found it }
        MOV     EAX,[ESI+2]

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$IFEND}

class function TObject.MethodAddress(const Name: string): Pointer;
begin
  Result := MethodAddress(UTF8EncodeToShortString(Name));
end;





class function TObject.MethodName(Address: Pointer): string;
{$IF not defined(CPU386)}
begin
end;
{$ELSE}
asm     //StackAlignSafe
        { ->    EAX     Pointer to class        }
        {       EDX     Address                 }
        {       ECX     Pointer to result       }
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EDI,ECX
        XOR     EBX,EBX
        XOR     ECX,ECX
        JMP     @@haveVMT
@@outer:
        MOV     EAX,[EAX]
@@haveVMT:
        MOV     ESI,[EAX].vmtMethodTable { fetch pointer to method table }
        TEST    ESI,ESI
        JE      @@parent
        MOV     CX,[ESI]
        TEST    ECX,ECX
        JZ      @@parent
        ADD     ESI,2
@@inner:
        CMP     EDX,[ESI+2]
        JE      @@found
        MOV     BX,[ESI]
        ADD     ESI,EBX
        DEC     ECX
        JNZ     @@inner
@@parent:
        MOV     EAX,[EAX].vmtParent
        TEST    EAX,EAX
        JNE     @@outer
        LEA     ESI,@@emptyStr
        JMP     @@exit

@@emptyStr:
        DB      0

@@found:
        ADD     ESI,6
@@exit:
        MOV     EAX,ESI
        MOV     EDX,EDI
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     UTF8ShortStringToString
end;
{$IFEND}





function TObject.FieldAddress(const Name: ShortString): Pointer;
{$IF not defined(CPU386)}
begin
end;
{$ELSE}
asm
        { ->    EAX     Pointer to instance     }
        {       EDX     Pointer to name         }
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        XOR     ECX,ECX
        XOR     EDI,EDI
        MOV     BL,[EDX]
        PUSH    EAX                     { save instance pointer         }

@@outer:
        MOV     EAX,[EAX]               { fetch class pointer           }
        MOV     ESI,[EAX].vmtFieldTable
        TEST    ESI,ESI
        JE      @@parent
        MOV     DI,[ESI]                { fetch count of fields         }
        TEST    EDI,EDI
        JZ      @@parent                { fieldExTab ref only           }
        ADD     ESI,6                   { count:U2 + classTab:P         }
@@inner:
        MOV     CL,[ESI+6]              { compare string lengths        }
        CMP     CL,BL
        JE      @@cmpChar
@@cont:
        LEA     ESI,[ESI+ECX+7]         { point ESI to next field       }
        DEC     EDI
        JNZ     @@inner
@@parent:
        MOV     EAX,[EAX].vmtParent     { fetch parent VMT              }
        TEST    EAX,EAX
        JNE     @@outer
        POP     EDX                     { forget instance, return Nil   }
        JMP     @@exit

@@notEqual:
        MOV     BL,[EDX]                { restore BL to length of name  }
        MOV     CL,[ESI+6]              { ECX := length of field name   }
        JMP     @@cont

@@utf8Cmp:
        PUSH    EAX
        PUSH    EDX
        LEA     EAX,[ESI+6]
        CALL    UTF8Compare
        XOR     ECX,ECX
        TEST    AL,AL
        POP     EDX
        POP     EAX
        JZ      @@notEqual
        JMP     @@foundIt

@@cmpChar:
        MOV     BL,[ESI+ECX+6]         { case insensitive string cmp    }
        TEST    BL,$80
        JNZ     @@utf8Cmp
        XOR     BL,[EDX+ECX+0]         { starting with last char        }
        TEST    BL,$80
        JNZ     @@utf8Cmp
        AND     BL,$DF
        JNE     @@notEqual
        DEC     ECX                     { ECX serves as counter         }
        JNZ     @@cmpChar

@@foundIt:
        { found it }
        MOV     EAX,[ESI]               { result is field offset plus ...   }
        POP     EDX
        ADD     EAX,EDX                 { instance pointer              }

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX

end;
{$IFEND}

function TObject.FieldAddress(const Name: string): Pointer;
begin
  Result := FieldAddress(UTF8EncodeToShortString(Name));
end;




{$IF not defined(CPU386)}
function _ClassCreate(InstanceOrVMT: Pointer; Alloc: ShortInt): TObject;
begin
  if Alloc >= 0 then
    InstanceOrVMT := Pointer(TClass(InstanceOrVMT).NewInstance);
  Result := TObject(InstanceOrVMT);
end;
{$ELSE CPU386}
function _ClassCreate(AClass: TClass; Alloc: Boolean): TObject;
asm
        { ->    EAX = pointer to VMT      }
        { <-    EAX = pointer to instance }
        PUSH    EDX
        PUSH    ECX
        PUSH    EBX
        TEST    DL,DL
        JL      @@noAlloc
        CALL    DWORD PTR [EAX] + VMTOFFSET TObject.NewInstance
@@noAlloc:
{$IFNDEF PC_MAPPED_EXCEPTIONS}
        XOR     EDX,EDX
        LEA     ECX,[ESP+16]
        MOV     EBX,FS:[EDX]
        MOV     [ECX].TExcFrame.next,EBX
        MOV     [ECX].TExcFrame.hEBP,EBP
        MOV     [ECX].TExcFrame.desc,offset @desc
        MOV     [ECX].TexcFrame.ConstructedObject,EAX   { trick: remember copy to instance }
        MOV     FS:[EDX],ECX
{$ENDIF PC_MAPPED_EXCEPTIONS}
        POP     EBX
        POP     ECX
        POP     EDX
        RET

{$IFNDEF PC_MAPPED_EXCEPTIONS}
@desc:
        JMP     _HandleAnyException

  {       destroy the object                                                      }

        MOV     EAX,[ESP+8+9*4]
        MOV     EAX,[EAX].TExcFrame.ConstructedObject
        TEST    EAX,EAX
        JE      @@skip
        MOV     ECX,[EAX]
        MOV     DL,$81
        PUSH    EAX
        CALL    DWORD PTR [ECX] + VMTOFFSET TObject.Destroy
        POP     EAX
        CALL    _ClassDestroy
@@skip:
  {       reraise the exception   }
        CALL    _RaiseAgain
{$ENDIF PC_MAPPED_EXCEPTIONS}
end;
{$IFEND CPU386}

procedure _ClassDestroy(Instance: TObject);
begin
  Instance.FreeInstance;
end;


function _AfterConstruction(Instance: TObject): TObject;
begin
  try
    Instance.AfterConstruction;
    Result := Instance;
  except
    _BeforeDestruction(Instance, 1);
    raise;
  end;
end;



{$IF not defined(CPU386)}
procedure _BeforeDestruction(Instance: TObject; OuterMost: ShortInt);
begin
  if OuterMost <= 0 then
    Instance.BeforeDestruction;
end;
{$ELSE CPU386}
function _BeforeDestruction(Instance: TObject; OuterMost: ShortInt): TObject;
// Must preserve DL on return!
{ The PUREPASCAL implementation below is disabled, because it is not able
  to preserve DL on return.  The compiler generated code depends on DL being
  preserved through the chain of calls up the inherited dtor chain.  Thus the
  PUREPASCAL implementation leads to non-deterministic results at destruction
  time with the current code generator.
}
//{$IFDEF PUREPASCAL}
//begin
//  Result := Instance;
//  if OuterMost > 0 then Exit;
//  Instance.BeforeDestruction;
//end;
//{$ELSE !PUREPASCAL}
asm //StackAlignSafe
       { ->  EAX  = pointer to instance }
       {      DL  = dealloc flag        }

        TEST    DL,DL
        JG      @@outerMost
        RET
@@outerMost:
{$IFDEF ALIGN_STACK}
        PUSH    ECX     // 4 byte adjustment, and ECX is convenient
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        MOV     EDX,[EAX]
        CALL    DWORD PTR [EDX] + VMTOFFSET TObject.BeforeDestruction
        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        POP     ECX     // 4 byte adjustment, and ECX is convenient
{$ENDIF ALIGN_STACK}
end;
//{$ENDIF !PUREPASCAL}
{$IFEND CPU386}

{ TMonitor }

{$IFDEF POSIX}
procedure Sleep(Timeout: Integer); inline;
begin
  usleep(Timeout * 1000);
end;

function GetTickCount: Cardinal; inline;
{$IFDEF LINUX}
var
  t: tms;
begin
  Result := Cardinal(Int64(Cardinal(times(t)) * 1000) div sysconf(_SC_CLK_TCK));
end;
{$ENDIF}
{$IFDEF MACOS}
begin
  Result := AbsoluteToNanoseconds(UpTime) div 1000000;
end;
{$ENDIF MACOS}

{$ENDIF POSIX}

{ TMonitor.TSpinWait }

procedure TMonitor.TSpinWait.Reset;
begin
  FCount := 0;
end;

procedure TMonitor.TSpinWait.SpinCycle;
var
  SpinCount: Integer;
begin
  if (FCount > YieldThreshold) or (CPUCount <= 1) then
  begin
    if FCount >= YieldThreshold then
      SpinCount := FCount - 10
    else
      SpinCount := FCount;
    if SpinCount mod Sleep1Threshold = Sleep1Threshold - 1 then
      Sleep(1)
    else if SpinCount mod Sleep0Threshold = Sleep0Threshold - 1 then
      Sleep(0)
    else
{$IFDEF MSWINDOWS}
      Yield;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
      sched_yield;
{$ENDIF POSIX}
  end else
    Spin(4 shl FCount);
  Inc(FCount);
  if FCount < 0 then
    FCount := 10;
end;

{ TMonitor.TSpinLock }

procedure TMonitor.TSpinLock.Enter;
var
  LLock: Integer;
  Wait: TSpinWait;
begin
  Wait.Reset;
  while True do
  begin
    LLock := FLock;
    if LLock = 0 then
    begin
      if InterlockedCompareExchange(FLock, 1, LLock) = LLock then
        System.Exit;
    end;
    Wait.SpinCycle;
  end;
end;

procedure TMonitor.TSpinLock.Exit;
begin
  InterlockedExchange(FLock, 0)
end;

class procedure TMonitor.Spin(Iterations: Integer);
begin
  while Iterations > 0 do
{$IFDEF CPU386}
  asm
    PAUSE
    DEC Iterations
  end;
{$ELSE}
    Dec(Iterations);
{$ENDIF}
end;

class procedure TMonitor.CheckMonitorSupport;
begin
  if MonitorSupport = nil then
    Error(reNoMonitorSupport);
end;

function TMonitor.CheckOwningThread: TThreadID;
begin
  Result := FOwningThread;
  if Result <> GetCurrentThreadId then
    Error(reMonitorNotLocked)
end;

class function TMonitor.Create: PMonitor;
begin
  New(Result);
  FillChar(Result^, SizeOf(Result^), 0);
end;

class procedure TMonitor.Destroy(AObject: TObject);
var
  MonitorFld: PPMonitor;
  Monitor: PMonitor;
begin
  MonitorFld := GetFieldAddress(AObject);
  if MonitorFld^ <> nil then
  begin
    Monitor := MonitorFld^;
    MonitorFld^ := nil;
    Monitor.Destroy;
  end;
end;

procedure TMonitor.Destroy;
begin
  if (MonitorSupport <> nil) and (FLockEvent <> nil) then
    MonitorSupport.FreeSyncObject(FLockEvent);
  Dispose(@Self);
end;

class procedure TMonitor.Enter(AObject: TObject);
begin
  CheckMonitorSupport;
  GetMonitor(AObject).Enter(INFINITE);
end;

class function TMonitor.Enter(AObject: TObject; Timeout: Cardinal): Boolean;
begin
  CheckMonitorSupport;
  Result := GetMonitor(AObject).Enter(Timeout);
end;

function TMonitor.DequeueWaiter: PWaitingThread;
begin
  FQueueLock.Enter;
  try
    Result := FWaitQueue;
    if (Result = nil) or (Result.Next = Result) then
    begin
      FWaitQueue := nil;
      System.Exit;
    end else
    begin
      Result := FWaitQueue.Next;
      FWaitQueue.Next := FWaitQueue.Next.Next;
    end;
  finally
    FQueueLock.Exit;
  end;
end;

function TMonitor.Enter(Timeout: Cardinal): Boolean;
label
  TryAgain;
var
  Done: Boolean;
  LockCount: Integer;
  StartCount, EndCount: Cardinal;
  SpinCount: Integer;
begin
  SpinCount := FSpinCount;
// Return here if signaled and lock wasn't acquired
TryAgain:
  Result := TryEnter;
  if not Result and (Timeout <> 0) then
  begin
    Done := False;
    // Get the spin count
    if SpinCount > 0 then
    begin
      StartCount := GetTickCount;
      while SpinCount > 0 do
      begin
        if (Timeout <> INFINITE) and ((GetTickCount - StartCount) >= Timeout) then
        begin
          Result := False;
          System.Exit;
        end;
        // if there are already waiters, don't bother spinning
        if FLockCount > 1 then
          Break;
        // Try to get the lock
        if FLockCount = 0 then
          if InterlockedCompareExchange(FLockCount, 1, 0) = 0 then
          begin
            FOwningThread := GetCurrentThreadId;
            FRecursionCount := 1;
            Result := True;
            System.Exit;
          end;
        {$IF defined(CPU386)}
        asm
          PAUSE // Just do nothing here...
        end;
        {$ELSE}
        YieldProcessor;
        {$IFEND}
        Dec(SpinCount);
        // Keep trying until the spin count expires
      end;
      // Adjust the timeout in case the spin-lock expired above.
      if Timeout <> INFINITE then
      begin
        EndCount := GetTickCount;
        if EndCount - StartCount >= Timeout then
        begin
          Result := False;
          System.Exit;
        end;
        Dec(Timeout, EndCount - StartCount);
      end;
    end;
    // Before we can block, we add our count to the lock
    while True do
    begin
      LockCount := FLockCount;
      if LockCount = 0 then
        goto TryAgain;
      if InterlockedCompareExchange(FLockCount, LockCount + 2, LockCount) = LockCount then
        Break;
    end;
    while True do
    begin
      StartCount := GetTickCount;
      // We're not the owner, so blocking is needed
      // GetEvent does a "safe" allocation of the Event
      Result := MonitorSupport.WaitOrSignalObject(nil, GetEvent, Timeout) = WAIT_OBJECT_0;
      if Timeout <> INFINITE then
      begin
        EndCount := GetTickCount;
        if EndCount - StartCount < Timeout then
          Dec(Timeout, EndCount - StartCount)
        else
          Timeout := 0;
      end;
      if Result then
      begin
        // Event was signaled, so try to acquire the lock since this could be a spurious condition
        while True do
        begin
          LockCount := FLockCount;
          if LockCount and 1 <> 0 then
            Break;
          if InterlockedCompareExchange(FLockCount, (LockCount - 2) or 1, LockCount) = LockCount then
          begin
            Done := True;
            Break;
          end;
        end;
      end else
      begin
        // We timed out, remove our presence from the lock count
        repeat
          LockCount := FLockCount;
        until InterlockedCompareExchange(FLockCount, LockCount - 2, LockCount) = LockCount;
        Done := True;
      end;
      if Done then
        Break;
    end;
    if Result then
    begin
      FOwningThread := GetCurrentThreadId;
      FRecursionCount := 1;
    end;
  end;
end;

procedure TMonitor.Exit;
var
  LockCount: Integer;
begin
  CheckOwningThread;
  Dec(FRecursionCount);
  if FRecursionCount = 0 then
  begin
    FOwningThread := 0;
    while True do
    begin
      LockCount := FLockCount;
      if InterlockedCompareExchange(FLockCount, LockCount - 1, LockCount) = LockCount then
      begin
        // if LockCount is <> 0 after we dropped our lock, there were waiters, so signal them
        if LockCount and not 1 <> 0 then
          MonitorSupport.WaitOrSignalObject(GetEvent, nil, 0);
        Break;
      end;
    end;
  end;
end;

class procedure TMonitor.Exit(AObject: TObject);
begin
  CheckMonitorSupport;
  GetMonitor(AObject).Exit;
end;

function TMonitor.GetEvent: Pointer;
var
  SleepTime: Integer;
  Event: Pointer;
begin
  SleepTime := 1;
  Result := FLockEvent;
  if Result = nil then
    while True do
    begin
      Event := MonitorSupport.NewSyncObject;
      Result := InterlockedCompareExchangePointer(FLockEvent, Event, nil);
      if Result = nil then
        // We won!  Nobody else was trying to allocate the Event.
        Result := Event
      else if Event <> nil then
        // Oh Well. We tried. Close the handle if someone got to it first.
        MonitorSupport.FreeSyncObject(Event);
      // Check if we actually were able to allocate the event without fail
      if Result <> nil then
        System.Exit;
      // We failed to allocate the event, so wait a bit to see if one becomes available
      Sleep(SleepTime);
      // Don't let it run-away, so return to a reasonable value and keep trying
      if SleepTime > 512 then
        SleepTime := 1
      else
        // Next time wait a little longer
        SleepTime := SleepTime shl 1;
    end;
end;

class function TMonitor.GetFieldAddress(AObject: TObject): PPMonitor;
begin
  Result := PPMonitor(NativeInt(AObject) + AObject.InstanceSize - hfFieldSize + hfMonitorOffset);
end;

class function TMonitor.GetMonitor(AObject: TObject): PMonitor;
var
  MonitorFld: PPMonitor;
  Monitor: PMonitor;
begin
  MonitorFld := GetFieldAddress(AObject);
  Result := MonitorFld^;
  if Result = nil then
  begin
    Monitor := TMonitor.Create;
    Result := InterlockedCompareExchangePointer(Pointer(MonitorFld^), Monitor, nil);
    if Result = nil then
      Result := Monitor
    else
      Dispose(Monitor);
  end;
end;

procedure TMonitor.Pulse;
var
  WaitingThread: PWaitingThread;
begin
  WaitingThread := DequeueWaiter;
  if WaitingThread <> nil then
    MonitorSupport.WaitOrSignalObject(WaitingThread.WaitEvent, nil, 0);
end;

class procedure TMonitor.Pulse(AObject: TObject);
begin
  CheckMonitorSupport;
  GetMonitor(AObject).Pulse;
end;

procedure TMonitor.PulseAll;
var
  WaitingThread: PWaitingThread;
begin
  WaitingThread := DequeueWaiter;
  while WaitingThread <> nil do
  begin
    MonitorSupport.WaitOrSignalObject(WaitingThread.WaitEvent, nil, 0);
    WaitingThread := DequeueWaiter;
  end;
end;

class procedure TMonitor.PulseAll(AObject: TObject);
begin
  CheckMonitorSupport;
  GetMonitor(AObject).PulseAll;
end;

procedure TMonitor.QueueWaiter(var WaitingThread: TWaitingThread);
begin
  FQueueLock.Enter;
  try
    if FWaitQueue = nil then
    begin
      FWaitQueue := @WaitingThread;
      WaitingThread.Next := @WaitingThread;
    end else
    begin
      WaitingThread.Next := FWaitQueue.Next;
      FWaitQueue.Next := @WaitingThread;
      FWaitQueue := @WaitingThread;
    end;
  finally
    FQueueLock.Exit;
  end;
end;

procedure TMonitor.RemoveWaiter(var WaitingThread: TWaitingThread);
var
  Last, Walker: PWaitingThread;
begin
  if FWaitQueue <> nil then
  begin
    FQueueLock.Enter;
    try
      Last := FWaitQueue.Next;
      Walker := Last.Next;
      while Walker <> FWaitQueue do
      begin
        if Walker = @WaitingThread then
        begin
          Last.Next := Walker.Next;
          Break;
        end;
        Last := Walker;
        Walker := Walker.Next;
      end;
      if (Walker = FWaitQueue) and (Walker = @WaitingThread) then
        if Walker.Next = Walker then
          FWaitQueue := nil
        else
          FWaitQueue := Last;
    finally
      FQueueLock.Exit;
    end;
  end;
end;



class procedure TMonitor.SetSpinCount(AObject: TObject; ASpinCount: Integer);
var
  Monitor: PMonitor;
begin
  if CPUCount > 1 then
  begin
    Monitor := GetMonitor(AObject);
    InterlockedExchange(Monitor.FSpinCount, ASpinCount);
  end;
end;

class function TMonitor.TryEnter(AObject: TObject): Boolean;
begin
  CheckMonitorSupport;
  Result := GetMonitor(AObject).TryEnter;
end;

function TMonitor.TryEnter: Boolean;
begin
  if FOwningThread = GetCurrentThreadId then  // check for recursion
  begin
    // Only the owning thread can increment this value so no need to guard it
    Inc(FRecursionCount);
    Result := True;
  // check to see if we can gain ownership
  end else if InterlockedCompareExchange(FLockCount, 1, 0) = 0 then
  begin
    //  Yep, got it.  Now claim ownership
    FOwningThread := GetCurrentThreadId;
    FRecursionCount := 1;
    Result := True;
  end else
    Result := False;
end;

function TMonitor.Wait(ALock: PMonitor; Timeout: Cardinal): Boolean;
var
  RecursionCount: Integer;
  WaitingThread: TWaitingThread;
begin
  WaitingThread.Next := nil;
  WaitingThread.Thread := ALock.CheckOwningThread;
  // This event should probably be cached someplace.
  // Probably not on the instance since this is a per-thread-per-instance resource
  WaitingThread.WaitEvent := MonitorSupport.NewWaitObject;
  try
    // Save the current recursion count for later
    RecursionCount := ALock.FRecursionCount;
    // Add the current thread to the waiting queue
    QueueWaiter(WaitingThread);
    // Set it back to almost released so the next Exit call actually drops the lock
    ALock.FRecursionCount := 1;
    // Now complete the exit and signal any waiters
    ALock.Exit;
    // Get in line for someone to do a Pulse or PulseAll
    Result := MonitorSupport.WaitOrSignalObject(nil, WaitingThread.WaitEvent, Timeout) = WAIT_OBJECT_0;
    // Got to get the lock back and block waiting for it.
    ALock.Enter(INFINITE);
    // Remove any dangling waiters from the list
    RemoveWaiter(WaitingThread);
    // Lets restore the recursion to return to the proper nesting level
    ALock.FRecursionCount := RecursionCount;
  finally
    MonitorSupport.FreeWaitObject(WaitingThread.WaitEvent);
  end;
end;

class function TMonitor.Wait(AObject: TObject; Timeout: Cardinal): Boolean;
var
  Monitor: PMonitor;
begin
  CheckMonitorSupport;
  Monitor := GetMonitor(AObject);
  Result := Monitor.Wait(Monitor, Timeout);
end;

class function TMonitor.Wait(AObject, ALock: TObject; Timeout: Cardinal): Boolean;
begin
  CheckMonitorSupport;
  Result := GetMonitor(AObject).Wait(GetMonitor(ALock), Timeout);
end;

function MonitorEnter(AObject: TObject; Timeout: Cardinal = INFINITE): Boolean;
begin
  Result := TMonitor.Enter(AObject, Timeout);
end;

function MonitorTryEnter(AObject: TObject): Boolean;
begin
  Result := TMonitor.TryEnter(AObject);
end;

procedure MonitorExit(AObject: TObject);
begin
  TMonitor.Exit(AObject);
end;

function MonitorWait(AObject: TObject; Timeout: Cardinal): Boolean;
begin
  Result := TMonitor.Wait(AObject, AObject, Timeout);
end;

function MonitorWait(AObject: TObject; ALock: TObject; Timeout: Cardinal): Boolean;
begin
  Result := TMonitor.Wait(AObject, ALock, Timeout);
end;

procedure MonitorPulse(AObject: TObject);
begin
  TMonitor.Pulse(AObject);
end;

procedure MonitorPulseAll(AObject: TObject);
begin
  TMonitor.PulseAll(AObject);
end;




procedure MemoryBarrier;
{$IF defined(CPU386)}
asm
      PUSH EAX
      XCHG [ESP],EAX
      POP  EAX
end;
{$ELSE}
begin
end;
{$IFEND}




procedure YieldProcessor;
{$IF defined(CPU386)}
asm
  PAUSE
end;
{$ELSE}
begin
end;
{$IFEND}

{
  The following NotifyXXXX routines are used to "raise" special exceptions
  as a signaling mechanism to an interested debugger.  If the debugger sets
  the DebugHook flag to 1 or 2, then all exception processing is tracked by
  raising these special exceptions.  The debugger *MUST* respond to the
  debug event with DBG_CONTINUE so that normal processing will occur.
}
{$IF defined(LINUX) or defined(MACOS)}
const
  excRaise      = 0; { an exception is being raised by the user (could be a reraise) }
  excCatch      = 1; { an exception is about to be caught }
  excFinally    = 2; { a finally block is about to be executed because of an exception }
  excUnhandled  = 3; { no user exception handler was found (the app will die) }

procedure _DbgExcNotify(
  NotificationKind: Integer;
  ExceptionObject: Pointer;
  ExceptionName: PShortString;
  ExceptionLocation: Pointer;
  HandlerAddr: Pointer); cdecl; export;
begin
{$IFDEF DEBUG}
  {
    This code is just for debugging the exception handling system.  The debugger
    needs _DbgExcNotify, however to place breakpoints in, so the function itself
    cannot be removed.
  }

  asm
{$IFDEF ALIGN_STACK}
    SUB  ESP, 8
{$ENDIF ALIGN_STACK}
    PUSH EAX
    PUSH EDX
  end;
  if Assigned(ExcNotificationProc) then
    ExcNotificationProc(NotificationKind, ExceptionObject, ExceptionName, ExceptionLocation, HandlerAddr);

  asm
    POP EDX
    POP EAX
{$IFDEF ALIGN_STACK}
    ADD  ESP, 8
{$ENDIF ALIGN_STACK}
  end;
{$ENDIF DEBUG}
end;

{
  The following functions are used by the debugger for the evaluator.  If you
  change them IN ANY WAY, the debugger will cease to function correctly.
}
procedure _DbgEvalMarker;
begin
end;

procedure _DbgEvalExcept(E: TObject);
begin
end;

procedure _DbgEvalEnd;
begin
end;

{
  This function is used by the debugger to provide a soft landing spot
  when evaluating a function call that may raise an unhandled exception.
  The return address of _DbgEvalMarker is pushed onto the stack so that
  the unwinder will transfer control to the except block.
}
procedure _DbgEvalFrame;
begin
  try
    _DbgEvalMarker;
  except on E: TObject do
    _DbgEvalExcept(E);
  end;
  _DbgEvalEnd;
end;

{
  These export names need to match the names that will be generated into
  the .symtab section, so that the debugger can find them if stabs
  debug information is being generated.
}
exports
  _DbgExcNotify   name  '@DbgExcNotify',
  _DbgEvalFrame   name  '@DbgEvalFrame',
  _DbgEvalMarker  name  '@DbgEvalMarker',
  _DbgEvalExcept  name  '@DbgEvalExcept',
  _DbgEvalEnd     name  '@DbgEvalEnd';
{$IFEND LINUX or MACOS}

{ tell the debugger that the next raise is a re-raise of the current non-Delphi
  exception }


{$IF defined(CPU386)}
procedure       NotifyReRaise;
asm
{$IFDEF PC_MAPPED_EXCEPTIONS}
{     ->EAX     Pointer to exception object }
{       EDX     location of exception       }
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    0                   { handler addr }
        PUSH    EDX                 { location of exception }
        MOV     ECX, [EAX]
        PUSH    [ECX].vmtClassName  { exception name }
        PUSH    EAX                 { exception object }
        PUSH    excRaise            { notification kind }
        CALL    _DbgExcNotify
{$IFDEF ALIGN_STACK}
        ADD     ESP, 28
{$ELSE !ALIGN_STACK}
        ADD     ESP, 20
{$ENDIF ALIGN_STACK}
{$ELSE !PC_MAPPED_EXCEPTIONS}
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    0
        PUSH    0
        PUSH    cContinuable
        PUSH    cDelphiReRaise
        CALL    RaiseExceptionProc
@@1:
{$ENDIF !PC_MAPPED_EXCEPTIONS}
end;
{$ELSE !CPU386}
procedure NotifyReRaise(Obj: TObject; Address: NativeUInt);
begin
  if DebugHook > 1 then
    RaiseExceptionProc(cDelphiReRaise, cContinuable, 0, nil);
end;
{$IFEND !CPU386}

{ tell the debugger about the raise of a non-Delphi exception }
{$IFDEF MSWINDOWS}


{$IF defined(CPU386)}
procedure       NotifyNonDelphiException;
asm
{     ->EAX     Pointer to exception object }
{       EDX     Context record              }
        CMP     BYTE PTR DebugHook,0
        JE      @@1
        PUSH    EAX
        PUSH    EAX
        PUSH    EDX
        PUSH    ESP
        PUSH    2
        PUSH    cContinuable
        PUSH    cNonDelphiException
        CALL    RaiseExceptionProc
        ADD     ESP,8
        POP     EAX
@@1:
end;
{$ELSE !CPU386}
procedure NotifyNonDelphiException(ExceptionObject: Pointer; ContextRecord: PContext);
var
  Param: array[0..1] of Pointer;
begin
  if DebugHook <> 0 then
  begin
    Param[0] := ContextRecord;
    Param[1] := ExceptionObject;
    RaiseExceptionProc(cNonDelphiException, cContinuable, 2, @Param);
  end;
end;
{$IFEND !CPU386}
{$ENDIF MSWINDOWS}

{ Tell the debugger where the handler for the current exception is located }


{$IF defined(CPU386)}
procedure NotifyExcept;
asm
{$IFDEF POSIX}
{     ->EAX     Pointer to exception object }
{       EDX     handler addr                }
        PUSH    EAX
        MOV     EAX, [EAX].TRaisedException.ExceptObject

{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EDX                 { handler addr }
        PUSH    0                   { location of exception }
        MOV     ECX, [EAX]
        PUSH    [ECX].vmtClassName  { exception name }
        PUSH    EAX                 { exception object }
        PUSH    excCatch            { notification kind }
        CALL    _DbgExcNotify
{$IFDEF ALIGN_STACK}
        ADD     ESP, 24
{$ELSE !ALIGN_STACK}
        ADD     ESP, 20
{$ENDIF ALIGN_STACK}

        POP     EAX
{$ELSE !POSIX}
        PUSH    ESP
        PUSH    1
        PUSH    cContinuable
        PUSH    cDelphiExcept           { our magic exception code }
        CALL    RaiseExceptionProc
        ADD     ESP,4
        POP     EAX
{$ENDIF !POSIX}
end;
{$ELSE !CPU386}
procedure NotifyExcept(HandlerAddress: NativeUInt);
begin
  RaiseExceptionProc(cDelphiExcept, cContinuable, 1, @HandlerAddress);
end;
{$IFEND !CPU386}



{$IF defined(CPU386)}
procedure NotifyOnExcept;
asm
{     ->EAX     Pointer to exception object }
{       EDX     handler addr                }
{$IFDEF PC_MAPPED_EXCEPTIONS}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EDX                 { handler addr }
        PUSH    0                   { location of exception }
        MOV     ECX, [EAX]
        PUSH    [ECX].vmtClassName  { exception name }
        PUSH    EAX                 { exception object }
        PUSH    excCatch            { notification kind }
        CALL    _DbgExcNotify
{$IFDEF ALIGN_STACK}
        ADD     ESP, 28
{$ELSE !ALIGN_STACK}
        ADD     ESP, 20
{$ENDIF ALIGN_STACK}
{$ELSE !PC_MAPPED_EXCEPTIONS}
{     ->EAX     Pointer to exception object             }
{       EBX     exception descriptor table entry        }
{       preserves EAX                                   }
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    EAX
        PUSH    [EBX].TExcDescEntry.handler
        JMP     NotifyExcept
@@1:
{$ENDIF !PC_MAPPED_EXCEPTIONS}
end;
{$ELSE !CPU386}
procedure NotifyOnExcept(HandlerAddress: NativeUInt);
begin
  if DebugHook > 1 then
    NotifyExcept(HandlerAddress);
end;
{$IFEND !CPU386}

{$IFDEF MSWINDOWS}


{$IF defined(CPU386)}
procedure NotifyAnyExcept;
asm
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    EAX
        PUSH    EBX
        JMP     NotifyExcept
@@1:
end;
{$ELSE !CPU386}
procedure NotifyAnyExcept(HandlerAddress: NativeUInt);
begin
  if DebugHook > 1 then
    NotifyExcept(HandlerAddress);
end;
{$IFEND !CPU386}
{$ENDIF}



{$IFDEF MSWINDOWS}
{$IF defined(CPU386)}
procedure       CheckJmp;
asm
        TEST    ECX,ECX
        JE      @@3
        MOV     EAX,[ECX + 1]
        CMP     BYTE PTR [ECX],0E9H { near jmp }
        JE      @@1
        CMP     BYTE PTR [ECX],0EBH { short jmp }
        JNE     @@3
        MOVSX   EAX,AL
        INC     ECX
        INC     ECX
        JMP     @@2
@@1:
        ADD     ECX,5
@@2:
        ADD     ECX,EAX
@@3:
end;
{$IFEND}
{$ENDIF MSWINDOWS}

{ Notify debugger of a finally during an exception unwind }


{$IF defined(CPU386)}
procedure NotifyExceptFinally;
asm
{     ->EAX     Pointer to exception object }
{       EDX     handler addr                }
{$IFDEF PC_MAPPED_EXCEPTIONS}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EDX                 { handler addr }
        PUSH    0                   { location of exception }
        PUSH    0                   { exception name }
        PUSH    0                   { exception object }
        PUSH    excFinally          { notification kind }
        CALL    _DbgExcNotify
{$IFDEF ALIGN_STACK}
        ADD     ESP, 28
{$ELSE !ALIGN_STACK}
        ADD     ESP, 20
{$ENDIF ALIGN_STACK}
{$ELSE !PC_MAPPED_EXCEPTIONS}
{     ->ECX     Pointer to exception object }
{       preserves: EAX, ECX and EDX.        }
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX
        CALL    CheckJmp
        PUSH    ECX
        PUSH    ESP                     { pass pointer to arguments }
        PUSH    1                       { there is 1 argument }
        PUSH    cContinuable            { continuable execution }
        PUSH    cDelphiFinally          { our magic exception code }
        CALL    RaiseExceptionProc
        POP     ECX
        POP     ECX
        POP     EDX
        POP     EAX
@@1:
{$ENDIF !PC_MAPPED_EXCEPTIONS}
end;
{$ELSE !CPU386}

procedure NotifyExceptFinally(TargetIp: NativeUInt);
begin
  if DebugHook > 1 then
    RaiseExceptionProc(cDelphiFInally, cContinuable, 1, @TargetIp);
end;
{$IFEND !CPU386}


{ Tell the debugger that the current exception is handled and cleaned up.
  Also indicate where execution is about to resume. }
{$IFDEF MSWINDOWS}



procedure       NotifyTerminate;
{$IF not defined(CPU386)}
begin
end;
{$ELSE}
asm
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    EDX
        PUSH    ESP
        PUSH    1
        PUSH    cContinuable
        PUSH    cDelphiTerminate        { our magic exception code }
        CALL    RaiseExceptionProc
        POP     EDX
@@1:
end;
{$IFEND}
{$ENDIF MSWINDOWS}

{ Tell the debugger that there was no handler found for the current exception
  and we are about to go to the default handler }



procedure       NotifyUnhandled;
{$IF not defined(CPU386)}
begin
end;
{$ELSE}
asm
{     ->EAX     Pointer to exception object }
{       EDX     location of exception       }
{$IFDEF MSWINDOWS}
        PUSH    EAX
        PUSH    EDX
        CMP     BYTE PTR DebugHook,1
        JBE     @@1
        PUSH    ESP
        PUSH    2
        PUSH    cContinuable
        PUSH    cDelphiUnhandled
        CALL    RaiseExceptionProc
@@1:
        POP     EDX
        POP     EAX
{$ELSE !MSWINDOWS}
        PUSH    EAX
        MOV     EAX, [EAX].TRaisedException.ExceptObject

{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    0                   { handler addr }
        PUSH    EDX                 { location of exception }
        MOV     ECX, [EAX]
        PUSH    [ECX].vmtClassName  { exception name }
        PUSH    EAX                 { exception object }
        PUSH    excUnhandled        { notification kind }
        CALL    _DbgExcNotify
{$IFDEF ALIGN_STACK}
        ADD     ESP, 24
{$ELSE !ALIGN_STACK}
        ADD     ESP, 20
{$ENDIF ALIGN_STACK}

        POP     EAX
{$ENDIF !MSWINDOWS}
end;
{$IFEND}

{$IFDEF PC_MAPPED_EXCEPTIONS}
//  MaybeCooptException
//    If a Delphi exception is thrown from C++, a TRaisedException object
//    will not be allocated yet on this side.  We need to keep things sane,
//    so we have to intercept such exceptions from the C++ side, and convert
//    them so that they appear to have been thrown from this RTL.  If we
//    throw a Delphi exception, then we set the private_2 member of
//    _Unwind_Exception to 0.  If C++ throws it, it sets it to the address
//    of the throw point.  We use this to distinguish the two cases, and
//    adjust data structures as appropriate.  On entry to this function,
//    EDX is the private_2 member, as set from SysRaiseException, and
//    EAX is the exception object in question.
//

procedure MaybeCooptException;
asm
        // If this exception is from C++, then private_2 will be a
        // throw address.  If not, then it will be zero.  private_1
        // will be either the exception object itself, or a TRaisedException.
        OR      EDX, EDX            // From C++?
        JZ      @@ExcAllocated

        // We've decided that the exception is from C++, but it is a
        // Delphi exception object.  We will coopt the exception now
        // by installing a TRaisedException into the unwinder exception,
        // and setting private_2 to 0.  Then the exception will look
        // like it was truly thrown from this RTL.
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    AllocateException
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
@@ExcAllocated:
end;


function LinkException(Exc: PRaisedException): PRaisedException;
asm //StackAlignSafe
        PUSH    EDX     // preserve EDX because of HandleOnException
        PUSH    EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    SysInit.@GetTLS
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        POP     EDX
        MOV     ECX, [EAX].ExceptionList
        MOV     [EDX].TRaisedException.Prev, ECX
        MOV     [EAX].ExceptionList, EDX
        MOV     EAX, EDX
        POP     EDX
end;


function UnlinkException: PRaisedException;
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    SysInit.@GetTLS
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        MOV     EDX, [EAX].ExceptionList
        MOV     EDX, [EDX].TRaisedException.Prev
        MOV     [EAX].ExceptionList, EDX
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

{$IF defined(MSWINDOWS) and (not defined(CPU386))}
const // for TExceptionRecord.ExceptionFlags
  EXCEPTION_NONCONTINUABLE  = $0001; // Noncontinuable exception
  EXCEPTION_UNWINDING       = $0002; // Unwind is in progress
  EXCEPTION_EXIT_UNWIND     = $0004; // Exit unwind is in progress
  EXCEPTION_STACK_INVALID   = $0008; // Stack out of limits or unaligned
  EXCEPTION_NESTED_CALL     = $0010; // Nested exception handler call
  EXCEPTION_TARGET_UNWIND   = $0020; // Execute termination handler for it
  EXCEPTION_COLLIDED_UNWIND = $0040; // unwind through unwind dispatcher
  EXCEPTION_UNWIND          = ( EXCEPTION_UNWINDING or
                                EXCEPTION_EXIT_UNWIND or
                                EXCEPTION_TARGET_UNWIND or
                                EXCEPTION_COLLIDED_UNWIND);
{$IFEND MSWINDOWS and (not CPU386)}

{$IF defined(MSWINDOWS) and (not defined(CPU386))}
function Dummy_GetExceptionObject(P: Pointer): TObject;
begin
  Result := TObject.Create;
end;
function Dummy_GetExceptionClass(P: Pointer): Pointer;
begin
  Result := Pointer(TObject);
end;

function FindOnExceptionDescEntry(DispatcherContext: PDispatcherContext;
  ExceptionClass: Pointer; ExcDesc: PExcDesc): PExcDescEntry;
var
  ExcDescEntry: PExcDescEntry;
  DescIndex: Integer;
  ClassPtr1, TabClassPtr: Pointer;
  Name1, Name2: PAnsiChar;
begin
  for DescIndex := 0 to ExcDesc^.DescCount - 1 do
  begin
    ExcDescEntry := @ExcDesc.DescTable[DescIndex];
    Result := ExcDescEntry;
    if ExcDescEntry^.VTable = 0 then
      Exit;
    ClassPtr1 := ExceptionClass;
    while True do
    begin
      TabClassPtr := PPointer(DispatcherContext.ImageBase + NativeUInt(ExcDescEntry^.VTable))^;
      if TabClassPtr = ClassPtr1 then
        Exit;
      if PInteger(NativeInt(TabClassPtr) + vmtInstanceSize)^ =
         PInteger(NativeInt(ClassPtr1) + vmtInstanceSize)^ then
      begin
        Name1 := PPAnsiChar(NativeInt(TabClassPtr) + vmtClassName)^;
        Name2 := PPAnsiChar(NativeInt(ClassPtr1) + vmtClassName)^;
        if (Name1^ = Name2^) and
           (_AStrCmp(PAnsiChar(NativeUInt(Name1) + 1),
                     PAnsiChar(NativeUInt(Name2) + 1), PByte(Name1)^) = 0) then
          Exit;
      end;
      ClassPtr1 := PPointer(NativeInt(ClassPtr1) + vmtParent)^;
      if ClassPtr1 = nil then
        break;
      ClassPtr1 := PPointer(ClassPtr1)^;
    end;
  end;
  Result := nil;
end;

function _X64DelphiExceptionHandler(
  ExceptionRecord: PExceptionRecord;
  EstablisherFrame: NativeUInt;
  ContextRecord: PContext;
  DispatcherContext: Pointer{PDispatcherContext}): LongInt{TExceptionDisposition};
var
  ImageBase: NativeUInt;
  ControlPcRVA: NativeUInt;
  ExceptionPointers: TExceptionPointers;
  ScopeIndex: Integer;
  DescIndex: Integer;
  TargetIpRVA: NativeUInt;
  ExceptionObject: Pointer;
  ExceptionClass: Pointer;
  ExceptionAddress: Pointer;
  ExcDescEntry: PExcDescEntry;
  ExcDesc: PExcDesc;
  ExcScope: PExcScope;
  ExcData: PExcData;
  TargetIp: NativeUInt;
  FilterRes: LongWord;
  JITCheckVal: Byte;
begin
//{

  ExceptObjProc := @Dummy_GetExceptionObject;
  ExceptClsProc := @Dummy_GetExceptionClass;
  RaiseExceptionProc := @RaiseException;
  RTLUnwindProc := @RTLUnwind;
//}

  Result := DISPOSITION_CONTINUE_SEARCH;
  ExcData := PExcData(PDispatcherContext(DispatcherContext).HandlerData);
  ImageBase := PDispatcherContext(DispatcherContext).ImageBase;
  ControlPcRVA := PDispatcherContext(DispatcherContext).ControlPc - ImageBase;

  if (ExceptionRecord.ExceptionFlags and EXCEPTION_UNWIND) = 0 then
  begin
    ExceptionPointers.ExceptionRecord := ExceptionRecord;
    ExceptionPointers.ContextRecord := ContextRecord;
    for ScopeIndex := 0 to ExcData^.ScopeCount - 1 do
    begin
      ExcScope := @ExcData^.ScopeTable[ScopeIndex];
      if (ControlPcRVA >= NativeUInt(ExcScope^.BeginOffset)) and
         (ControlPcRVA < NativeUInt(ExcScope^.EndOffset)) and
         (ExcScope^.TableOffset <> 0) then // catch_any or catch_table
      begin
        ExcDescEntry := nil;
        if ExcScope^.TableOffset > 1 then
        begin
          // catch_table
          if ExceptionRecord.ExceptionCode = cDelphiException then
            ExceptionClass := PPointer(ExceptionRecord.ExceptObject)^
          else
          begin
            if not Assigned(ExceptClsProc) then
              Continue;
            ExceptionClass := TExceptClsProc(ExceptClsProc)(ExceptionRecord);
            if ExceptionClass = nil then
              Continue;
          end;
          ExcDesc := PExcDesc(ImageBase + NativeUInt(ExcScope^.TableOffset));
          ExcDescEntry := FindOnExceptionDescEntry(PDispatcherContext(DispatcherContext),
                                                   ExceptionClass, ExcDesc);
          if ExcDescEntry = nil then
            Continue;
        end;

        if ExceptionRecord.ExceptionCode = cDelphiException then
        begin
          //ExceptionObject := ExceptionRecord.ExceptObject;
          //ExceptionAddress := ExceptionRecord.ExceptAddr;
          JITCheckVal := 1;
        end
        else
        begin
          if not Assigned(ExceptObjProc) then
            Continue;
          ExceptionObject := TExceptObjProc(ExceptObjProc)(ExceptionRecord);
          if ExceptionObject = nil then
            Continue;
          //ExceptionAddress := ExceptionRecord.ExceptionAddress;
          JITCheckVal := 1;
          if ExceptionRecord.ExceptionCode <> cCppException then
          begin
            NotifyNonDelphiException(ExceptionObject, ContextRecord);
            JITCheckVal := 0;
          end;
        end;
        if (JITEnable > JITCheckVal) and (DebugHook <= 0) then
        begin
          FilterRes := UnhandledExceptionFilter(@ExceptionPointers);
          if FilterRes = EXCEPTION_CONTINUE_SEARCH then
            Continue;
        end;

        ExceptionRecord.ExceptionFlags :=
          ExceptionRecord.ExceptionFlags or EXCEPTION_UNWINDING;
        TargetIp := ImageBase  + NativeUInt(ExcScope^.EndOffset);
        RtlUnwindEx(EstablisherFrame, TargetIp,
                    ExceptionRecord,
                    NativeInt(ExceptionRecord.ExceptionCode),
                    PDispatcherContext(DispatcherContext).ContextRecord,
                    PDispatcherContext(DispatcherContext).HistoryTable);
      end;
    end;
  end
  else
  begin
    TargetIpRVA := PDispatcherContext(DispatcherContext).TargetIp - ImageBase;
    for ScopeIndex := 0 to ExcData^.ScopeCount - 1 do
    begin
      ExcScope := @ExcData^.ScopeTable[ScopeIndex];
      if (ControlPcRVA >= NativeUInt(ExcScope^.BeginOffset)) and
         (ControlPcRVA < NativeUInt(ExcScope^.EndOffset)) then
      begin
        if (TargetIpRVA >= NativeUInt(ExcScope^.BeginOffset)) and
           (TargetIpRVA < NativeUInt(ExcScope^.EndOffset)) and
           ((ExceptionRecord.ExceptionFlags and EXCEPTION_TARGET_UNWIND) <> 0) then
          Exit;
        ExceptionObject := ExceptionRecord.ExceptObject;
        ExceptionAddress := ExceptionRecord.ExceptAddr;
{disabled until Obj.Free is implemented
        // Make the RaiseList entry on the stack
        NewRaiseList.NextRaise := RaiseListPtr;
        NewRaiseList.ExceptAddr := ExceptionAddress;
        NewRaiseList.ExceptObject := TObject(ExceptionObject);
        NewRaiseList.ExceptionRecord := ExceptionRecord;
        RaiseListPtr := @NewRaiseList;
}
        ExceptionPointers.ExceptionRecord := ExceptionRecord;
        ExceptionPointers.ContextRecord := ContextRecord;
        case ExcScope^.TableOffset of
        0: // finally block
          begin
            //if TargetIpRVA = ExcScope^.TargetOffset then
            //  Exit; // DISPOSITION_CONTINUE_SEARCH
            PDispatcherContext(DispatcherContext).ControlPc := ImageBase + NativeUInt(ExcScope^.EndOffset);
            TargetIp := ImageBase + NativeUInt(ExcScope^.TargetOffset);
            NotifyExceptFinally(TargetIp);
            _TDelphiFinallyHandlerProc(TargetIp)(@ExceptionPointers, EstablisherFrame);
{disabled until Obj.Free is implemented
            // we come here if an exception handler has thrown yet another exception
            // we need to destroy the exception object and pop the raise list.
            Obj := PRaiseFrame(RaiseListPtr)^.ExceptObject;
            RaiseListPtr := PRaiseFrame(RaiseListPtr)^.NextRaise;
            Obj.Free;
}
          end;
        1: // catch block
          begin
            NotifyAnyExcept(TargetIp);
            TargetIp := ImageBase + NativeUInt(ExcScope^.TargetOffset);
            TargetIp := _TDelphiCatchHandlerProc(TargetIp)(@ExceptionPointers, EstablisherFrame, ExceptionObject);
          end;
        else // catch table
          ExcDesc := PExcDesc(ImageBase + NativeUInt(ExcScope^.TableOffset));
          ExcDescEntry := FindOnExceptionDescEntry(PDispatcherContext(DispatcherContext),
                                                   ExceptionClass, ExcDesc);
          if ExcDescEntry = nil then
            Continue;
          NotifyOnExcept(TargetIp);
          TargetIp := ImageBase + ExcDescEntry^.Handler;
          TargetIp := _TDelphiCatchHandlerProc(TargetIp)(@ExceptionPointers, EstablisherFrame, ExceptionObject);
        end;
      end;
    end;
  end;
end;
{$IFEND MSWINDOWS and (not CPU386)}

{$IF defined(MSWINDOWS) and defined(CPU386)}
procedure _HandleFinallyInternal; forward;
{$IFEND MSWINDOWS and CPU386}

{
 When an exception is to be handled unconditionally by some bit of user code,
 there is still some book-keeping that needs to be done.  There is special
 handling that needs to be done if the exception is from C++, either a Delphi
 exception, or a pure C++ exception.  We have to restore some internal state
 as well, and we have to notify the debugger.  Once we've done those things,
 we return to the exception handling fragment in the user code that called us.
}

{$IF defined(CPU386)}
procedure _HandleAnyException;
asm //StackAlignSafe
{$IFDEF PC_MAPPED_EXCEPTIONS}
        CMP     ECX, UW_EXC_CLASS_BORLANDCPP    // C++ exception?
        JNE     @@handleIt                      // nope, handle it
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        // C++ exceptions aren't wanted here.  Retoss them as is
        // We won't return from this.
        CALL    SysRaiseCPPException

@@handleIt:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        CALL    UnblockOSExceptions
        POP     EDX
        POP     EAX

{$IFDEF ALIGN_STACK}
        // We'll just increase our alignment adjustment from above
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        // If the exception is a Delphi exception thrown from C++, coopt it.
        CALL    MaybeCooptException

        OR      [EAX].TRaisedException.Flags, excIsBeingHandled
        CALL    LinkException
        MOV     ESI, EBX
        MOV     EDX, [ESP] // EDX = return address
        CALL    NotifyExcept
        MOV     EBX, ESI
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
        {
         When _HandleAnyException was called, the stack was adjusted to align
         it per the Mach ABI.  The unwinder will have reset the stack pointer
         to the prologue result, and the generated code in the caller will
         have further adjusted for dynamic arrays.  Thus the alignment adjustment
         made by the compiler will always have been 12 bytes.  The code generator
         could dispose of that in the user code, on return, but it saves a little
         space, and it's easy to do from here.
        }
//        RET   12
{$ENDIF ALIGN_STACK}
        // End of routine - we return to user code now.
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFNDEF  PC_MAPPED_EXCEPTIONS}
        { ->    [ESP+ 4] excPtr: PExceptionRecord       }
        {       [ESP+ 8] errPtr: PExcFrame              }
        {       [ESP+12] ctxPtr: Pointer                }
        {       [ESP+16] dspPtr: Pointer                }
        { <-    EAX return value - always one           }

        MOV     EAX,[ESP+4]
        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JNE     @@exit

        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        MOV     EDX,[EAX].TExceptionRecord.ExceptObject
        MOV     ECX,[EAX].TExceptionRecord.ExceptAddr
        JE      @@DelphiException
        CLD
        CALL    _FpuInit
        MOV     EDX,ExceptObjProc
        TEST    EDX,EDX
        JE      @@exit
        CALL    EDX
        TEST    EAX,EAX
        JE      @@exit
        MOV     EDX,[ESP+12]
        MOV     ECX,[ESP+4]
        CMP     [ECX].TExceptionRecord.ExceptionCode,cCppException
        JE      @@CppException
        CALL    NotifyNonDelphiException
        CMP     BYTE PTR JITEnable,0
        JBE     @@CppException
        CMP     BYTE PTR DebugHook,0
        JA      @@CppException                     // Do not JIT if debugging
        LEA     ECX,[ESP+4]
        PUSH    EAX
        PUSH    ECX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        POP     EAX
        JE      @@exit
        MOV     EDX,EAX
        MOV     EAX,[ESP+4]
        MOV     ECX,[EAX].TExceptionRecord.ExceptionAddress
        JMP     @@GoUnwind

@@CppException:
        MOV     EDX,EAX
        MOV     EAX,[ESP+4]
        MOV     ECX,[EAX].TExceptionRecord.ExceptionAddress

@@DelphiException:
        CMP     BYTE PTR JITEnable,1
        JBE     @@GoUnwind
        CMP     BYTE PTR DebugHook,0                { Do not JIT if debugging }
        JA      @@GoUnwind
        PUSH    EAX
        LEA     EAX,[ESP+8]
        PUSH    EDX
        PUSH    ECX
        PUSH    EAX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        POP     ECX
        POP     EDX
        POP     EAX
        JE      @@exit

@@GoUnwind:
        OR      [EAX].TExceptionRecord.ExceptionFlags,cUnwinding

        PUSH    EBX
        XOR     EBX,EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EBX,FS:[EBX]
        PUSH    EBX                     { Save pointer to topmost frame }
        PUSH    EAX                     { Save OS exception pointer     }
        PUSH    EDX                     { Save exception object         }
        PUSH    ECX                     { Save exception address        }

        MOV     EDX,[ESP+8+8*4]

        PUSH    0
        PUSH    EAX
        PUSH    offset @@returnAddress
        PUSH    EDX
        CALL    RtlUnwindProc
@@returnAddress:

        MOV     EDI,[ESP+8+8*4]

        {       Make the RaiseList entry on the stack }

        CALL    SysInit.@GetTLS
        PUSH    [EAX].RaiseListPtr
        MOV     [EAX].RaiseListPtr,ESP

        MOV     EBP,[EDI].TExcFrame.hEBP
        MOV     EBX,[EDI].TExcFrame.desc
        MOV     [EDI].TExcFrame.desc,offset @@exceptFinally

        ADD     EBX,TExcDesc.instructions
        CALL    NotifyAnyExcept
        JMP     EBX

@@exceptFinally:
        JMP     _HandleFinallyInternal

@@destroyExcept:
        {       we come here if an exception handler has thrown yet another exception }
        {       we need to destroy the exception object and pop the raise list. }

        CALL    SysInit.@GetTLS
        MOV     ECX,[EAX].RaiseListPtr
        MOV     EDX,[ECX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,EDX

        MOV     EAX,[ECX].TRaiseFrame.ExceptObject
        JMP     TObject.Free

@@exit:
        MOV     EAX,1
{$ENDIF !PC_MAPPED_EXCEPTIONS}  { not PC_MAPPED_EXCEPTIONS }
end;
{$IFEND CPU386}

{$IFDEF PC_MAPPED_EXCEPTIONS}
{
  Common code between the Win32 and PC mapped exception handling
  scheme.  This function takes a pointer to an object, and an exception
  'on' descriptor table and finds the matching handler descriptor.

  For support of Linux, we assume that EBX has been loaded with the GOT
  that pertains to the code which is handling the exception currently.
  If this function is being called from code which is not PIC, then
  EBX should be zero on entry.

  N.B. For the Mac, it is critical that this code never calls out of
  the System unit, as we do not align the stack around calls to it.
}

procedure FindOnExceptionDescEntry;
asm
{ ->    EAX raised object: Pointer                }
{       EDX descriptor table: ^TExcDesc           }
{       EBX GOT of user code, or 0 if not an SO   }
{ <-    EAX matching descriptor: ^TExcDescEntry   }
        PUSH    EBP
        MOV     EBP, ESP
        SUB     ESP, 8                          { Room for vtable temp, and adjustor }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV [EBP - 8], EBX      { Store the potential GOT }
        MOV EAX, [EAX]          { load vtable of exception object }
        MOV     EBX,[EDX].TExcDesc.cnt
        LEA     ESI,[EDX].TExcDesc.excTab       { point ECX to exc descriptor table }
        MOV     [EBP - 4], EAX                  { temp for vtable of exception object }

@@innerLoop:
        MOV     EAX,[ESI].TExcDescEntry.vTable
        TEST    EAX,EAX                         { catch all clause?                     }
        JE      @@found                         { yes: This is the handler              }
        ADD     EAX, [EBP - 8]                  { add in the adjustor (could be 0) }
        MOV     EDI,[EBP - 4]                   { load vtable of exception object       }
        JMP     @@haveVMT

@@vtLoop:
        MOV     EDI,[EDI]
@@haveVMT:
        MOV     EAX,[EAX]
        CMP     EAX,EDI
        JE      @@found

        MOV     ECX,[EAX].vmtInstanceSize
        CMP     ECX,[EDI].vmtInstanceSize
        JNE     @@parent

        MOV     EAX,[EAX].vmtClassName
        MOV     EDX,[EDI].vmtClassName

        XOR     ECX,ECX
        MOV     CL,[EAX]
        CMP     CL,[EDX]
        JNE     @@parent

        INC     EAX
        INC     EDX
{$IFDEF ALIGN_STACK}
        SUB      ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    _AStrCmp
{$IFDEF ALIGN_STACK}
        ADD      ESP, 4
{$ENDIF ALIGN_STACK}
        JE      @@found

@@parent:
        MOV     EDI,[EDI].vmtParent             { load vtable of parent         }
        MOV     EAX,[ESI].TExcDescEntry.vTable
        ADD     EAX, [EBP - 8]                  { add in the adjustor (could be 0) }
        TEST    EDI,EDI
        JNE     @@vtLoop

        ADD     ESI,8
        DEC     EBX
        JNZ     @@innerLoop

        { Didn't find a handler. }
        XOR     ESI, ESI

@@found:
        MOV     EAX, ESI
@@done:
        POP     EDI
        POP     ESI
        POP     EBX
        MOV     ESP, EBP
        POP     EBP
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

{$IF defined(CPU386)}
{$IFDEF PC_MAPPED_EXCEPTIONS}

procedure       _HandleOnExceptionPIC;
asm
        { ->    EAX obj : Exception object }
        {       [RA]  desc: ^TExcDesc }
        { <-    Doesn't return }

        // Mark the exception as being handled
        OR      [EAX].TRaisedException.Flags, excIsBeingHandled

        MOV     ESI, EBX                      // Save the GOT
        MOV     EDX, [ESP]                    // Get the addr of the TExcDesc
        PUSH    EAX                           // Save the object
        MOV     EAX, [EAX].TRaisedException.ExceptObject
        CALL    FindOnExceptionDescEntry
        OR      EAX, EAX
        JE      @@NotForMe

        MOV     EBX, ESI                      // Set back to user's GOT
        MOV     EDX, EAX
        POP     EAX                           // Get the object back
        POP     ECX                           // Ditch the return addr

        CALL    LinkException

        // Get the Pascal object itself.
        MOV     EAX, [EAX].TRaisedException.ExceptObject

        MOV     EDX, [EDX].TExcDescEntry.handler
        ADD     EDX, EBX                      // adjust for GOT
        CALL    NotifyOnExcept

        MOV     EBX, ESI                      // Make sure of user's GOT
        JMP     EDX                           // Back to the user code
        // never returns
@@NotForMe:
        POP     EAX                           // Get the exception object

        // Mark that we're reraising this exception, so that the
        // compiler generated exception handler for the 'except on' clause
        // will not get confused
        OR      [EAX].TRaisedException.Flags, excIsBeingReRaised

        JMP     SysRaiseException             // Should be using resume here
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFEND CPU386}

{
 N.B.  On the Mac, the stack alignment code in _HandleOnException is fine tuned.
 On first read, it may seem incorrect.
}

{$IF defined(CPU386)}
procedure       _HandleOnException;
{$IFDEF PC_MAPPED_EXCEPTIONS}
asm //StackAlignSafe
        { ->    EAX obj : Exception object }
        {       [RA]  desc: ^TExcDesc }
        { <-    Doesn't return }

        CMP     ECX, UW_EXC_CLASS_BORLANDCPP    // C++ exception?
        JNE     @@handleIt                      // nope, handle it
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        // C++ exceptions aren't wanted here.  Retoss them as is.
        // We won't return from this.
        CALL    SysRaiseCPPException

@@handleIt:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        // If the exception is a Delphi exception thrown from C++, coopt it.
        CALL    MaybeCooptException

        // Mark the exception as being handled
        OR      [EAX].TRaisedException.Flags, excIsBeingHandled

{$IFDEF ALIGN_STACK}
        MOV     EDX, [ESP + 12]               // Get the addr of the TExcDesc
{$ELSE !ALIGN_STACK}
        MOV     EDX, [ESP]                    // Get the addr of the TExcDesc
{$ENDIF !ALIGN_STACK}
        // STACK: 16 - no alignment required by FindOnExceptionDescEntry
        // N.B.  We have to make sure that FindOnExceptionDescEntry
        // never calls out of System.pas.
        PUSH    EAX                           // Save the object
        PUSH    EBX                           // Save EBX
        XOR     EBX, EBX                      // No GOT
        MOV     EAX, [EAX].TRaisedException.ExceptObject
        CALL    FindOnExceptionDescEntry
        POP     EBX                           // Restore EBX
        OR      EAX, EAX                      // Is the exception for me?
        JE      @@NotForMe

        MOV     EDX, EAX
        POP     EAX                           // Get the object back
{$IFDEF ALIGN_STACK}
        ADD     ESP, 16                       // Ditch the alignment _and_ return addr
        // STACK ALIGNMENT: 16, since we ditched the return addr as well
{$ELSE !ALIGN_STACK}
        POP     ECX                           // Ditch the return addr
{$ENDIF !ALIGN_STACK}

        CALL    LinkException

        // Get the Pascal object itself.
        MOV     EAX, [EAX].TRaisedException.ExceptObject

        MOV     EDX, [EDX].TExcDescEntry.handler
        CALL    NotifyOnExcept                // Tell the debugger about it

{$IFDEF ALIGN_STACK}
        {
         When _HandleOnException was called, the stack was adjusted to align
         it per the Mach ABI.  The unwinder will have reset the stack pointer
         to the prologue result, and the generated code in the caller will
         have further adjusted for dynamic arrays.  Thus the alignment adjustment
         made by the compiler will always have been 12 bytes.  We have to
         discard that alignment here.
        }
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        JMP     EDX                           // Back to the user code
        // never returns
@@NotForMe:
        POP     EAX                           // Get the exception object
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}

        // Mark that we're reraising this exception, so that the
        // compiler generated exception handler for the 'except on' clause
        // will not get confused
        OR      [EAX].TRaisedException.Flags, excIsBeingReRaised
        JMP     SysRaiseException             // Should be using resume here
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}


{$IFNDEF  PC_MAPPED_EXCEPTIONS}
asm
        { ->    [ESP+ 4] excPtr: PExceptionRecord       }
        {       [ESP+ 8] errPtr: PExcFrame              }
        {       [ESP+12] ctxPtr: Pointer                }
        {       [ESP+16] dspPtr: Pointer                }
        { <-    EAX return value - always one           }

        MOV     EAX,[ESP+4]
        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JNE     @@exit

        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        JE      @@DelphiException
        CLD
        CALL    _FpuInit
        MOV     EDX,ExceptClsProc
        TEST    EDX,EDX
        JE      @@exit
        CALL    EDX
        TEST    EAX,EAX
        JNE     @@common
        JMP     @@exit

@@DelphiException:
        MOV     EAX,[EAX].TExceptionRecord.ExceptObject
        MOV     EAX,[EAX]                       { load vtable of exception object       }

@@common:
        MOV     EDX,[ESP+8]

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     ECX,[EDX].TExcFrame.desc
        MOV     EBX,[ECX].TExcDesc.cnt
        LEA     ESI,[ECX].TExcDesc.excTab       { point ECX to exc descriptor table }
        MOV     EBP,EAX                         { load vtable of exception object }

@@innerLoop:
        MOV     EAX,[ESI].TExcDescEntry.vTable
        TEST    EAX,EAX                         { catch all clause?                     }
        JE      @@doHandler                     { yes: go execute handler               }
        MOV     EDI,EBP                         { load vtable of exception object       }
        JMP     @@haveVMT

@@vtLoop:
        MOV     EDI,[EDI]
@@haveVMT:
        MOV     EAX,[EAX]
        CMP     EAX,EDI
        JE      @@doHandler

        MOV     ECX,[EAX].vmtInstanceSize
        CMP     ECX,[EDI].vmtInstanceSize
        JNE     @@parent

        MOV     EAX,[EAX].vmtClassName
        MOV     EDX,[EDI].vmtClassName

        XOR     ECX,ECX
        MOV     CL,[EAX]
        CMP     CL,[EDX]
        JNE     @@parent

        INC     EAX
        INC     EDX
        CALL    _AStrCmp
        JE      @@doHandler

@@parent:
        MOV     EDI,[EDI].vmtParent             { load vtable of parent         }
        MOV     EAX,[ESI].TExcDescEntry.vTable
        TEST    EDI,EDI
        JNE     @@vtLoop

        ADD     ESI,8
        DEC     EBX
        JNZ     @@innerLoop

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     @@exit

@@doHandler:
        MOV     EAX,[ESP+4+4*4]
        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        MOV     EDX,[EAX].TExceptionRecord.ExceptObject
        MOV     ECX,[EAX].TExceptionRecord.ExceptAddr
        JE      @@haveObject
        CALL    ExceptObjProc
        MOV     EDX,[ESP+12+4*4]
        CALL    NotifyNonDelphiException
        CMP     BYTE PTR JITEnable,0
        JBE     @@NoJIT
        CMP     BYTE PTR DebugHook,0
        JA      @@noJIT                 { Do not JIT if debugging }
        LEA     ECX,[ESP+4+4*4]
        PUSH    EAX
        PUSH    ECX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        POP     EAX
        JE      @@exit

@@noJIT:
        MOV     EDX,EAX
        MOV     EAX,[ESP+4+4*4]
        MOV     ECX,[EAX].TExceptionRecord.ExceptionAddress
        JMP     @@GoUnwind

@@haveObject:
        CMP     BYTE PTR JITEnable,1
        JBE     @@GoUnwind
        CMP     BYTE PTR DebugHook,0
        JA      @@GoUnwind
        PUSH    EAX
        LEA     EAX,[ESP+4+5*4]
        PUSH    EDX
        PUSH    ECX
        PUSH    EAX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        POP     ECX
        POP     EDX
        POP     EAX
        JE      @@exit

@@GoUnwind:
        XOR     EBX,EBX
        MOV     EBX,FS:[EBX]
        PUSH    EBX                     { Save topmost frame     }
        PUSH    EAX                     { Save exception record  }
        PUSH    EDX                     { Save exception object  }
        PUSH    ECX                     { Save exception address }

        MOV     EDX,[ESP+8+8*4]
        OR      [EAX].TExceptionRecord.ExceptionFlags,cUnwinding

        PUSH    ESI                     { Save handler entry     }

        PUSH    0
        PUSH    EAX
        PUSH    offset @@returnAddress
        PUSH    EDX
        CALL    RtlUnwindProc
@@returnAddress:

        POP     EBX                     { Restore handler entry  }

        MOV     EDI,[ESP+8+8*4]

        {       Make the RaiseList entry on the stack }

        CALL    SysInit.@GetTLS
        PUSH    [EAX].RaiseListPtr
        MOV     [EAX].RaiseListPtr,ESP

        MOV     EBP,[EDI].TExcFrame.hEBP
        MOV     [EDI].TExcFrame.desc,offset @@exceptFinally
        MOV     EAX,[ESP].TRaiseFrame.ExceptObject
        CALL    NotifyOnExcept
        JMP     [EBX].TExcDescEntry.handler

@@exceptFinally:
        JMP     _HandleFinallyInternal

@@destroyExcept:
        {       we come here if an exception handler has thrown yet another exception }
        {       we need to destroy the exception object and pop the raise list. }

        CALL    SysInit.@GetTLS
        MOV     ECX,[EAX].RaiseListPtr
        MOV     EDX,[ECX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,EDX

        MOV     EAX,[ECX].TRaiseFrame.ExceptObject
        JMP     TObject.Free
@@exit:
        MOV     EAX,1
end;
{$ENDIF !PC_MAPPED_EXCEPTIONS}
{$IFEND CPU386}


{$IF defined(CPU386)}
procedure _HandleFinally;
{$IFDEF PC_MAPPED_EXCEPTIONS}
asm //StackAlignSafe
{$IFDEF PIC}
        MOV     ESI, EBX
{$ENDIF PIC}
        CMP     ECX, UW_EXC_CLASS_BORLANDCPP    // C++ exception?
        JNE     @@handleIt                      // nope, handle it
        // unwinding a C++ exception.  We handle that specially.
        PUSH    EAX
        PUSH    EDX
        PUSH    ECX
        MOV     EDX, [ESP+12]
        CALL    EDX
        POP     ECX
        POP     EDX
        POP     EAX
        CALL    SysRaiseCPPException

@@handleIt:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4 { RA, XX, and 2 PUSHes to come = 16}
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        CALL    UnblockOSExceptions
        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8 { RA, prev XX, XX XX = 16 }
{$ENDIF ALIGN_STACK}

        // If the exception is a Delphi exception thrown from C++, coopt it.
        CALL    MaybeCooptException

{$IFDEF ALIGN_STACK}
        MOV     EDX, [ESP + 12] { get the return address; stack still aligned }
{$ELSE !ALIGN_STACK}
        MOV     EDX, [ESP]
{$ENDIF !ALIGN_STACK}
        CALL    NotifyExceptFinally
        PUSH    EAX
{$IFDEF PIC}
        MOV     EBX, ESI
{$ENDIF PIC}
        {
          Mark the current exception with the EBP of the handler.  If
          an exception is raised from the finally block, then this
          exception will be orphaned.  We will catch this later, when
          we clean up the next except block to complete execution.
          See DoneExcept.
        }
        MOV [EAX].TRaisedException.HandlerEBP, EBP
        { Note: This call appears misalligned when ALIGN_STACK is
          defined, but the finally handler fixes up the stack upon
          entry.  This is due to the fact that the code generated for
          the finally block deals with both the normal flow, and the
          exception case.  In the normal flow, we push the address of
          a label to simulate a return address, prior to entering the
          finally block.  This push mis-aligns the stack, and the
          finally block compensates.  We'll be skipping that push,
          so the return address that we pushed above mirrors the effect. }
        CALL    EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        { We have to make it look like we've arrived here and setup
          a basic EBP frame, in order for the unwind that we will now
          cause to succeed properly.  We popped the saved EAX, now
          we have to get rid of stuff up to the original return address. }
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        {
          We executed the finally handler without adverse reactions.
          It's safe to clear the marker now.
        }
        MOV [EAX].TRaisedException.HandlerEBP, $FFFFFFFF
        PUSH    EBP
        MOV     EBP, ESP
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    SysRaiseException             // Should be using resume here
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF MSWINDOWS}
asm
        { ->    [ESP+ 4] excPtr: PExceptionRecord       }
        {       [ESP+ 8] errPtr: PExcFrame              }
        {       [ESP+12] ctxPtr: Pointer                }
        {       [ESP+16] dspPtr: Pointer                }
        { <-    EAX return value - always one           }

        MOV     EAX,[ESP+4]
        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JE      @@exit

        PUSH    EBX
        XOR     EBX,EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        {       Make exception frame    }

        PUSH    EBP
        PUSH    offset @@exceptFinally
        PUSH    dword ptr FS:[EBX]
        MOV     FS:[EBX],ESP

        MOV     EBX,FS:[EBX]
        MOV     EDX,[EAX].TExceptionRecord.ExceptObject
        MOV     ECX,[EAX].TExceptionRecord.ExceptAddr
        PUSH    EBX                     { Save pointer to topmost frame }
        PUSH    EAX                     { Save OS exception pointer     }
        PUSH    EDX                     { Save exception object         }
        PUSH    ECX                     { Save exception address        }

        MOV     EDI,[ESP+8+11*4]        { Load errPtr:PExcFrame         }

        {       Make the RaiseList entry on the stack   }

        CALL    SysInit.@GetTLS
        PUSH    [EAX].RaiseListPtr
        MOV     [EAX].RaiseListPtr,ESP

        MOV     ECX,[EDI].TExcFrame.desc
        MOV     EBP,[EDI].TExcFrame.hEBP
        MOV     [EDI].TExcFrame.desc,offset @@exceptFinally
        ADD     ECX,TExcDesc.instructions
        CALL    NotifyExceptFinally
        CALL    ECX

        CALL    SysInit.@GetTLS
        MOV     ECX,[EAX].RaiseListPtr
        MOV     EDX,[ECX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,EDX
        ADD     ESP,5*4                 { Remove local RaiseList        }

        {       Remove exception frame  }

        XOR     EAX,EAX
        POP     EDX
        POP     ECX
        POP     ECX
        MOV     FS:[EAX],EDX

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     @@exit

@@exceptFinally:
        JMP     _HandleFinallyInternal

@@destroyExcept:
        {       we come here if an finalization handler has thrown yet  }
        {       another exception we need to destroy the exception      }
        {       object and pop the raise list.                          }

        CALL    SysInit.@GetTLS
        MOV     ECX,[EAX].RaiseListPtr
        MOV     EDX,[ECX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,EDX

        MOV     EAX,[ECX].TRaiseFrame.ExceptObject
        JMP     TObject.Free

@@exit:
        MOV     EAX,1
end;
{$ENDIF MSWINDOWS}
{$IFEND !CPU386}



{$IF defined(CPU386)}
{$IFDEF MSWINDOWS}
procedure       _HandleFinallyInternal;
asm
        { ->    [ESP+ 4] excPtr: PExceptionRecord       }
        {       [ESP+ 8] errPtr: PExcFrame              }
        {       [ESP+12] ctxPtr: Pointer                }
        {       [ESP+16] dspPtr: Pointer                }
        { <-    EAX return value - always one           }

        MOV     EAX,[ESP+4]
        MOV     EDX,[ESP+8]
        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JE      @@exit
        MOV     ECX,[EDX].TExcFrame.desc
        MOV     [EDX].TExcFrame.desc,offset @@exit

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EBP,[EDX].TExcFrame.hEBP
        ADD     ECX,TExcDesc.instructions
        CALL    NotifyExceptFinally
        CALL    ECX

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX

@@exit:
        MOV     EAX,1
end;
{$ENDIF MSWINDOWS}
{$IFEND CPU386}




procedure       _HandleAutoException;
{$IF not defined(CPU386)}
begin
end;
{$ELSE CPU386}
{$IFNDEF PC_MAPPED_EXCEPTIONS}
{$IFDEF MSWINDOWS}
asm
        { ->    [ESP+ 4] excPtr: PExceptionRecord       }
        {       [ESP+ 8] errPtr: PExcFrame              }
        {       [ESP+12] ctxPtr: Pointer                }
        {       [ESP+16] dspPtr: Pointer                }
        { <-    EAX return value - always one           }

        MOV     EAX,[ESP+4]
        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JNE     @@exit

        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        CLD
        CALL    _FpuInit
        JE      @@DelphiException
        CMP     BYTE PTR JITEnable,0
        JBE     @@DelphiException
        CMP     BYTE PTR DebugHook,0
        JA      @@DelphiException

@@DoUnhandled:
        LEA     EAX,[ESP+4]
        PUSH    EAX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        JE      @@exit
        MOV     EAX,[ESP+4]
        JMP     @@GoUnwind

@@DelphiException:
        CMP     BYTE PTR JITEnable,1
        JBE     @@GoUnwind
        CMP     BYTE PTR DebugHook,0
        JA      @@GoUnwind
        JMP     @@DoUnhandled

@@GoUnwind:
        OR      [EAX].TExceptionRecord.ExceptionFlags,cUnwinding

        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EDX,[ESP+8+3*4]

        PUSH    0
        PUSH    EAX
        PUSH    offset @@returnAddress
        PUSH    EDX
        CALL    RtlUnwindProc

@@returnAddress:
        POP     EBP
        POP     EDI
        POP     ESI
        MOV     EAX,[ESP+4]
        MOV     EBX,8000FFFFH
        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        JNE     @@done

        MOV     EDX,[EAX].TExceptionRecord.ExceptObject
        MOV     ECX,[EAX].TExceptionRecord.ExceptAddr
        MOV     EAX,[ESP+8]
        MOV     EAX,[EAX].TExcFrame.SelfOfMethod
        TEST    EAX,EAX
        JZ      @@freeException
        MOV     EBX,[EAX]
        CALL    DWORD PTR [EBX] + VMTOFFSET TObject.SafeCallException
        MOV     EBX,EAX
@@freeException:
        MOV     EAX,[ESP+4]
        MOV     EAX,[EAX].TExceptionRecord.ExceptObject
        CALL    TObject.Free
@@done:
        XOR     EAX,EAX
        MOV     ESP,[ESP+8]
        POP     ECX
        MOV     FS:[EAX],ECX
        POP     EDX
        POP     EBP
        LEA     EDX,[EDX].TExcDesc.instructions
        POP     ECX
        JMP     EDX
@@exit:
        MOV     EAX,1
end;
{$ELSE !MSWINDOWS}
begin
  Error(reSafeCallError);  //!!
end;
{$ENDIF !MSWINDOWS}
{$ELSE PC_MAPPED_EXCEPTIONS}
asm
        // EAX = TObject reference, or nil
        // [ESP] = ret addr

{$IFDEF ALIGN_STACK}
         SUB      ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        CALL    UnblockOSExceptions

        CALL    CurrentException
        MOV     EDX, [ESP]
        // If the exception is a Delphi exception thrown from C++, coopt it.
        CALL    MaybeCooptException
        CALL    LinkException
        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD      ESP, 4
{$ENDIF ALIGN_STACK}

//  The compiler wants the stack to look like this:
//  ESP+4->  HRESULT
//  ESP+0->  ret addr
//
//  Make it so.
//
        POP     EDX
        PUSH    8000FFFFH
        PUSH    EDX

        OR      EAX, EAX    // Was this a method call?
        JE      @@Done

{$IFDEF ALIGN_STACK}
        SUB      ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        CALL    CurrentException
        MOV     EDX, [EAX].TRaisedException.ExceptObject
        MOV     ECX, [EAX].TRaisedException.ExceptionAddr;
        POP     EAX
        MOV     EAX, [EAX]
{$IFDEF ALIGN_STACK}
        SUB      ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    DWORD PTR [EAX] + VMTOFFSET TObject.SafeCallException;
{$IFDEF ALIGN_STACK} //12 = outer alignment
        MOV     [ESP+12], EAX
        ADD      ESP, 8
{$ELSE !ALIGN_STACK}
        MOV     [ESP+4], EAX
{$ENDIF !ALIGN_STACK}
@@Done:
{$IFDEF ALIGN_STACK}
        SUB      ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    _DoneExcept
{$IFDEF ALIGN_STACK}
        ADD      ESP, 8
{$ENDIF ALIGN_STACK}
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFEND CPU386}

{$IF defined(CPU386)}
{$IFDEF PC_MAPPED_EXCEPTIONS}


procedure       _RaiseAtExcept;
asm //StackAlignSafe
        { ->    EAX     Pointer to exception object     }
        { ->    EDX     Purported addr of exception     }
        { Be careful: EBX is not set up in PIC mode. }
        { Outward bound calls must go through an exported fn, like SysRaiseException }
        OR      EAX, EAX
        JNE     @@GoAhead
        MOV     EAX, 216
        JMP     _RunError

@@GoAhead:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    BlockOSExceptions
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EBP
        MOV     EBP, ESP
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    NotifyReRaise
        CALL    AllocateException
{$IFDEF PIC}
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX, EAX
        POP     EAX
        MOV     EDX, [EBX].RaiseExceptObjProc
        MOV     EDX, [EDX]
{$ELSE !PIC}
        MOV     EDX,RaiseExceptObjProc
{$ENDIF !PIC}
        TEST    EDX,EDX
        JZ      @@DoRaise
{$IFDEF ALIGN_STACK}
        MOV     [ESP],EAX
        CALL    EDX
        MOV     EAX,[ESP]
{$ELSE}
        PUSH    EAX
        CALL    EDX
        POP     EAX
{$ENDIF ALIGN_STACK}
@@DoRaise:
        CALL    SysRaiseException
        {
          This can only return if there was a terrible error.  In this event,
          we have to bail out.
        }
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        JMP     _Run0Error
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$ELSE !CPU386}
procedure _RaiseAtExcept(Obj: TObject; Address: NativeUInt);
var
  Params: Array[0..6] of NativeUInt;
  ExceptionRecord: TExceptionRecord;
  CurRaiseList : PRaiseFrame;
begin
  if OBJ = nil then
    _RunError(216); // reAccessViolation

  NotifyReRaise(Obj, Address);

  Params[0] := Address;
  Params[1] := NativeUInt(Pointer(Obj));
  Params[2] := 0 {EBX};
  Params[3] := 0 {ESI};
  Params[4] := 0 {EDI};
  Params[5] := 0 {EBP};
  Params[6] := 0 {ESP};

  if RaiseExceptObjProc <> nil then
  begin
    CurRaiseList := PRaiseFrame(RaiseListPtr);
    ExceptionRecord.ExceptionCode := cDelphiException;
    ExceptionRecord.ExceptionFlags := cNonContinuable;
    ExceptionRecord.ExceptionRecord := nil;
    if CurRaiseList <> nil then
      ExceptionRecord.ExceptionRecord := CurRaiseList^.ExceptionRecord;
    ExceptionRecord.ExceptionAddress := Pointer(Address);
    ExceptionRecord.NumberParameters := 7;
    ExceptionRecord.ExceptionInformation[0] := Params[0];
    ExceptionRecord.ExceptionInformation[1] := Params[1];
    ExceptionRecord.ExceptionInformation[2] := Params[2];
    ExceptionRecord.ExceptionInformation[3] := Params[3];
    ExceptionRecord.ExceptionInformation[4] := Params[4];
    ExceptionRecord.ExceptionInformation[5] := Params[5];
    ExceptionRecord.ExceptionInformation[6] := Params[6];
    TRaiseExceptObjProc(RaiseExceptObjProc)(@ExceptionRecord);
  end;
  RaiseExceptionProc(cDelphiException, cNonContinuable, 7, @Params);
end;
{$IFEND !CPU386}



{$IF defined(CPU386)}
procedure       _RaiseExcept;
{$IFDEF PC_MAPPED_EXCEPTIONS}
asm
        { ->    EAX     Pointer to exception object     }
        MOV     EDX, [ESP]
        JMP     _RaiseAtExcept
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF MSWINDOWS}
asm
  { When making changes to the way Delphi Exceptions are raised, }
  { please realize that the C++ Exception handling code reraises }
  { some exceptions as Delphi Exceptions.  Of course we want to  }
  { keep exception raising compatible between Delphi and C++, so }
  { when you make changes here, consult with the relevant C++    }
  { exception handling engineer. The C++ code is in xx.cpp, in   }
  { the RTL sources, in function tossAnException.                }

  { ->    EAX     Pointer to exception object     }
  {       [ESP]   Error address           }

        OR      EAX, EAX
        JNE     @@GoAhead
        MOV     EAX, 216
        CALL    _RunError
@@GoAhead:
        POP     EDX

        PUSH    ESP
        PUSH    EBP
        PUSH    EDI
        PUSH    ESI
        PUSH    EBX
        PUSH    EAX                             { pass class argument           }
        PUSH    EDX                             { pass address argument         }

        MOV     EAX,ESP                         { Need these values later }
        PUSH    ESP                             { pass pointer to arguments             }
        PUSH    7                               { there are seven arguments               }
        PUSH    cNonContinuable                 { we can't continue execution   }
        PUSH    cDelphiException                { our magic exception code              }
        PUSH    EDX                             { pass the user's return address        }
        MOV     EDX,RaiseExceptObjProc          { has this been hooked? }
        TEST    EDX,EDX
        JZ      @@2

        PUSH    [EAX + 6 * 4]
        PUSH    [EAX + 5 * 4]
        PUSH    [EAX + 4 * 4]
        PUSH    [EAX + 3 * 4]
        PUSH    [EAX + 2 * 4]
        PUSH    [EAX + 1 * 4]                   { object }
        PUSH    [EAX + 0 * 4]                   { address }
        PUSH    7                               { how many of the above }
        PUSH    [EAX + 0 * 4]                   { the address goes here again }
        PUSH    EAX
        PUSH    EDX
        CALL    RaiseList
        MOV     ECX,EAX
        POP     EDX
        POP     EAX
        TEST    ECX,ECX
        JZ      @@1
        MOV     ECX,[ECX].TRaiseFrame.ExceptionRecord
@@1:    PUSH    ECX
        PUSH    cNonContinuable
        PUSH    cDelphiException
        MOV     EAX,ESP
        CALL    EDX
        ADD     ESP,12 * 4                      { Cleanup 12 DWORDS from the stack }
@@2:
        JMP     RaiseExceptionProc
end;
{$ENDIF MSWINDOWS}
{$ELSE !CPU386}

procedure _RaiseExcept(Obj: TObject);
var
  Address: NativeUInt;
begin
  Address := PNativeUInt(NativeUInt(@Address) + $10)^;
  _RaiseAtExcept(Obj, Address);
end;
{$IFEND !CPU386}

{$IFDEF PC_MAPPED_EXCEPTIONS}
{
  Used in the PC mapping exception implementation to handle exceptions in constructors.
}

procedure       _ClassHandleException;
asm
  {
  EAX = Ptr to TRaisedException
  EDX = self
  ECX = top flag
  }
        PUSH     ECX
{$IFDEF ALIGN_STACK}
        SUB      ESP, 8
{$ENDIF ALIGN_STACK}
        CALL     LinkException
{$IFDEF ALIGN_STACK}
        ADD      ESP, 8
{$ENDIF ALIGN_STACK}
        MOV      EAX,EDX
        POP      EDX
        TEST     DL, DL
        JE       _RaiseAgain
        MOV      ECX,[EAX]
        MOV      DL,$81
{$IFDEF ALIGN_STACK}
        SUB      ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH     EAX
        CALL     DWORD PTR [ECX] + VMTOFFSET TObject.Destroy
        POP      EAX
{$IFDEF ALIGN_STACK}
        SUB      ESP, 4
{$ENDIF ALIGN_STACK}
        CALL     _ClassDestroy
{$IFDEF ALIGN_STACK}
        ADD      ESP, 12
{$ENDIF ALIGN_STACK}
        JMP      _RaiseAgain
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}




procedure       _RaiseAgain;
{$IF not defined(CPU386)}
begin
end;
{$ELSE}
{$IFDEF PC_MAPPED_EXCEPTIONS}
asm
{$IFDEF ALIGN_STACK}
        SUB      ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    CurrentException
{$IFDEF ALIGN_STACK}
        ADD      ESP, 12
{$ENDIF ALIGN_STACK}
// The following notifies the debugger of a reraise of exceptions.  This will
// be supported in a later release, but is disabled for now.
//        PUSH    EAX
//        MOV     EDX, [EAX].TRaisedException.ExceptionAddr
//        MOV     EAX, [EAX].TRaisedException.ExceptObject
//        CALL    NotifyReRaise                   { Tell the debugger }
//        POP     EAX
        TEST    [EAX].TRaisedException.Flags, excIsBeingHandled
        JZ      @@DoIt
        OR      [EAX].TRaisedException.Flags, excIsBeingReRaised
@@DoIt:
        PUSH    EAX
{$IFDEF ALIGN_STACK}
        SUB      ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    UnlinkException
{$IFDEF ALIGN_STACK}
        ADD      ESP, 8
{$ENDIF ALIGN_STACK}
        POP     EAX
        MOV     EDX, [ESP]                      { Get the user's addr }
        JMP     SysRaiseException
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF MSWINDOWS}
asm
        { ->    [ESP        ] return address to user program }
        {       [ESP+ 4     ] raise list entry (4 dwords)    }
        {       [ESP+ 4+ 4*4] saved topmost frame            }
        {       [ESP+ 4+ 5*4] saved registers (4 dwords)     }
        {       [ESP+ 4+ 9*4] return address to OS           }
        { ->    [ESP+ 4+10*4] excPtr: PExceptionRecord       }
        {       [ESP+ 8+10*4] errPtr: PExcFrame              }

        { Point the error handler of the exception frame to something harmless }

        MOV     EAX,[ESP+8+10*4]
        MOV     [EAX].TExcFrame.desc,offset @@exit

        { Pop the RaiseList }

        CALL    SysInit.@GetTLS
        MOV     EDX,[EAX].RaiseListPtr
        MOV     ECX,[EDX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,ECX

        { Destroy any objects created for non-delphi exceptions }

        MOV     EAX,[EDX].TRaiseFrame.ExceptionRecord
        AND     [EAX].TExceptionRecord.ExceptionFlags,NOT cUnwinding
        CMP     [EAX].TExceptionRecord.ExceptionCode,cDelphiException
        JE      @@delphiException
        MOV     EAX,[EDX].TRaiseFrame.ExceptObject
        CALL    TObject.Free
        CALL    NotifyReRaise

@@delphiException:

        XOR     EAX,EAX
        ADD     ESP,5*4
        MOV     EDX,FS:[EAX]
        POP     ECX
        MOV     EDX,[EDX].TExcFrame.next
        MOV     [ECX].TExcFrame.next,EDX

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
@@exit:
        MOV     EAX,1
end;
{$ENDIF MSWINDOWS}
{$IFEND}

{$IFDEF PC_MAPPED_EXCEPTIONS}
{
  This is implemented slow and dumb.  The theory is that it is rare
  to throw an exception past an except handler, and that the penalty
  can be particularly high here.  Partly it's done the dumb way for
  the sake of maintainability.  It could be inlined.
}

procedure       _DestroyException;
var
  Exc: PRaisedException;
  RefCount: Integer;
  ExcObj: Pointer;
  ExcAddr: Pointer;
begin
  asm
        CMP     ECX, UW_EXC_CLASS_BORLANDCPP
        JNE     @@notCPP
{$IFDEF ALIGN_STACK}
        SUB      ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    SysRaiseCPPException
{$IFDEF ALIGN_STACK}
        ADD      ESP, 12
{$ENDIF ALIGN_STACK}
@@notCPP:
    MOV     Exc, EAX
  end;

  if (Exc^.Flags and excIsBeingReRaised) = 0 then
  begin
    RefCount := Exc^.RefCount;
    ExcObj := Exc^.ExceptObject;
    ExcAddr := Exc^.ExceptionAddr;
    Exc^.RefCount := 1;
    FreeException;
    _DoneExcept;
    Exc := AllocateException(ExcObj, ExcAddr);
    Exc^.RefCount := RefCount;
  end;

  Exc^.Flags := Exc^.Flags and not (excIsBeingReRaised or excIsBeingHandled);

  SysRaiseException(Exc);
end;


{ cleanup of old PC_MAPPED_EXCEPTION code, This routine doesn't appear to be
 used any more, and should be considered for removal}
procedure CleanupException;
asm
        CALL    FreeException
        OR      EAX, EAX
        JE      @@Done
        CALL    TObject.Free
@@Done:
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}



procedure       _DoneExcept;
{$IF not defined(CPU386)}
begin
end;
{$ELSE}
{$IFDEF PC_MAPPED_EXCEPTIONS}
asm //StackAlignSafe
{$IFDEF ALIGN_STACK}
        {
          We do one alignment call for the entire function as there are no
          other stack adjustments made in the function body.
        }
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    FreeException
        OR      EAX, EAX
        JE      @@Done
        CALL    TObject.Free
@@Done:
        CALL    UnlinkException
        {
          Take a peek at the next exception object on the stack.
          If its EBP marker is at an address lower than our current
          EBP, then we know that it was orphaned when an exception was
          thrown from within the execution of a finally block.  We clean
          it up now, so that we won't leak exception records/objects.
        }
        CALL    CurrentException
        OR      EAX, EAX
        JE      @@Done2
        CMP     [EAX].TRaisedException.HandlerEBP, EBP
        JA      @@Done2
        CALL    FreeException
        OR      EAX, EAX
        JE      @@Done2
        CALL    TObject.Free
@@Done2:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF MSWINDOWS}
asm
        { ->    [ESP+ 4+10*4] excPtr: PExceptionRecord       }
        {       [ESP+ 8+10*4] errPtr: PExcFrame              }

        { Pop the RaiseList }

        CALL    SysInit.@GetTLS
        MOV     EDX,[EAX].RaiseListPtr
        MOV     ECX,[EDX].TRaiseFrame.NextRaise
        MOV     [EAX].RaiseListPtr,ECX

        { Destroy exception object }

        MOV     EAX,[EDX].TRaiseFrame.ExceptObject
        CALL    TObject.Free

        POP     EDX
        MOV     ESP,[ESP+8+9*4]
        XOR     EAX,EAX
        POP     ECX
        MOV     FS:[EAX],ECX
        POP     EAX
        POP     EBP
        CALL    NotifyTerminate
        JMP     EDX
end;
{$ENDIF MSWINDOWS}
{$IFEND}

{$IFNDEF PC_MAPPED_EXCEPTIONS}



{$IFDEF MSWINDOWS}
procedure   _TryFinallyExit;
{$IF not defined(CPU386)}
begin
end;
{$ELSE}
asm
        XOR     EDX,EDX
        MOV     ECX,[ESP+4].TExcFrame.desc
        MOV     EAX,[ESP+4].TExcFrame.next
        ADD     ECX,TExcDesc.instructions
        MOV     FS:[EDX],EAX
{$IFDEF ALIGN_STACK}
        SUB      ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    ECX
{$IFDEF ALIGN_STACK}
        ADD      ESP, 12
{$ENDIF ALIGN_STACK}
@@1:    RET     12
end;
{$IFEND}
{$ENDIF MSWINDOWS}
{$ENDIF !PC_MAPPED_EXCEPTIONS}

{$IFNDEF PC_MAPPED_EXCEPTIONS}

procedure       MapToRunError(P: PExceptionRecord); stdcall;
const
  STATUS_ACCESS_VIOLATION         = $C0000005;
  STATUS_ARRAY_BOUNDS_EXCEEDED    = $C000008C;
  STATUS_FLOAT_DENORMAL_OPERAND   = $C000008D;
  STATUS_FLOAT_DIVIDE_BY_ZERO     = $C000008E;
  STATUS_FLOAT_INEXACT_RESULT     = $C000008F;
  STATUS_FLOAT_INVALID_OPERATION  = $C0000090;
  STATUS_FLOAT_OVERFLOW           = $C0000091;
  STATUS_FLOAT_STACK_CHECK        = $C0000092;
  STATUS_FLOAT_UNDERFLOW          = $C0000093;
  STATUS_INTEGER_DIVIDE_BY_ZERO   = $C0000094;
  STATUS_INTEGER_OVERFLOW         = $C0000095;
  STATUS_PRIVILEGED_INSTRUCTION   = $C0000096;
  STATUS_STACK_OVERFLOW           = $C00000FD;
  STATUS_CONTROL_C_EXIT           = $C000013A;
var
  ErrCode: Byte;
begin
  case P.ExceptionCode of
    STATUS_INTEGER_DIVIDE_BY_ZERO:  ErrCode := 200; { reDivByZero }
    STATUS_ARRAY_BOUNDS_EXCEEDED:   ErrCode := 201; { reRangeError }
    STATUS_FLOAT_OVERFLOW:          ErrCode := 205; { reOverflow }
    STATUS_FLOAT_INEXACT_RESULT,
    STATUS_FLOAT_INVALID_OPERATION,
    STATUS_FLOAT_STACK_CHECK:       ErrCode := 207; { reInvalidOp }
    STATUS_FLOAT_DIVIDE_BY_ZERO:    ErrCode := 200; { reZeroDivide }
    STATUS_INTEGER_OVERFLOW:        ErrCode := 215; { reIntOverflow}
    STATUS_FLOAT_UNDERFLOW,
    STATUS_FLOAT_DENORMAL_OPERAND:  ErrCode := 206; { reUnderflow }
    STATUS_ACCESS_VIOLATION:        ErrCode := 216; { reAccessViolation }
    STATUS_PRIVILEGED_INSTRUCTION:  ErrCode := 218; { rePrivInstruction }
    STATUS_CONTROL_C_EXIT:          ErrCode := 217; { reControlBreak }
    STATUS_STACK_OVERFLOW:          ErrCode := 202; { reStackOverflow }
  else                              ErrCode := 255;
  end;
  RunErrorAt(ErrCode, P.ExceptionAddress);
end;




procedure       _ExceptionHandler;
{$IF not defined(CPU386)}
begin
end;
{$ELSE CPU386}
asm
        MOV     EAX,[ESP+4]

        TEST    [EAX].TExceptionRecord.ExceptionFlags,cUnwindInProgress
        JNE     @@exit
{$IFDEF MSWINDOWS}
        CMP     BYTE PTR DebugHook,0
        JA      @@ExecuteHandler
        LEA     EAX,[ESP+4]
        PUSH    EAX
        CALL    UnhandledExceptionFilter
        CMP     EAX,EXCEPTION_CONTINUE_SEARCH
        //JNE     @@ExecuteHandler
        //JMP     @@exit
        JE      @@exit
{$ENDIF MSWINDOWS}

@@ExecuteHandler:
        MOV     EAX,[ESP+4]
        CLD
        CALL    _FpuInit
        MOV     EDX,[ESP+8]

        PUSH    0
        PUSH    EAX
        PUSH    offset @@returnAddress
        PUSH    EDX
        CALL    RtlUnwindProc

@@returnAddress:
        MOV     EBX,[ESP+4]
        CMP     [EBX].TExceptionRecord.ExceptionCode,cDelphiException
        MOV     EDX,[EBX].TExceptionRecord.ExceptAddr
        MOV     EAX,[EBX].TExceptionRecord.ExceptObject
        JE      @@DelphiException2

        MOV     EDX,ExceptObjProc
        TEST    EDX,EDX
        JE      MapToRunError
        MOV     EAX,EBX
        CALL    EDX
        TEST    EAX,EAX
        JE      MapToRunError
        MOV     EDX,[EBX].TExceptionRecord.ExceptionAddress

@@DelphiException2:

        CALL    NotifyUnhandled
        MOV     ECX,ExceptProc
        TEST    ECX,ECX
        JE      @@noExceptProc
        CALL    ECX             { call ExceptProc(ExceptObject, ExceptAddr) }

@@noExceptProc:
        MOV     ECX,[ESP+4]
        MOV     EAX,217
        MOV     EDX,[ECX].TExceptionRecord.ExceptAddr
        MOV     [ESP],EDX
        JMP     _RunError

@@exit:
        XOR     EAX,EAX
end;
{$IFEND CPU386}




procedure       SetExceptionHandler(Context: PInitContext);
{$IF not defined(CPU386)}
begin
end;
{$ELSE}
asm
        { ->    EAX   PInitContext
        { ->    [EBP-type(TExcFrame)] TExcFrame local (returned in EAX) }

        PUSH    EAX               { Save off Context pointer }
        XOR     EDX,EDX           { using [EDX] saves some space over [0] }
        LEA     EAX,[EBP-type(TExcFrame)]
        MOV     ECX,FS:[EDX]      { ECX := head of chain                  }
        MOV     FS:[EDX],EAX      { head of chain := @exRegRec            }

        MOV     [EAX].TExcFrame.next,ECX
{$IFDEF PIC}
        LEA     EDX,[EBX]._ExceptionHandler
        MOV     [EAX].TExcFrame.desc,EDX
{$ELSE}
        MOV     [EAX].TExcFrame.desc,offset _ExceptionHandler
{$ENDIF}
        MOV     [EAX].TExcFrame.hEBP,EBP
        POP     ECX               { Restore Context pointer }
        MOV     [ECX].TInitContext.ExcFrame,EAX
end;
{$IFEND}




procedure       UnsetExceptionHandler(Context: PInitContext);
{$IF not defined(CPU386)}
begin
end;
{$ELSE}
asm
        { ->    EAX   PInitContext }

        MOV     EAX,[EAX].TInitContext.ExcFrame
        XOR     EDX,EDX
        TEST    EAX,EAX
        JZ      @@exit

        MOV     ECX,FS:[EDX]    { ECX := head of chain          }
        CMP     EAX,ECX         { simple case: our record is first      }
        JNE     @@search
        MOV     EAX,[EAX]       { head of chain := exRegRec.next        }
        MOV     FS:[EDX],EAX
        JMP     @@exit

@@loop:
        MOV     ECX,[ECX]
@@search:
        CMP     ECX,-1          { at end of list?                       }
        JE      @@exit          { yes - didn't find it          }
        CMP     [ECX],EAX       { is it the next one on the list?       }
        JNE     @@loop          { no - look at next one on list }
@@unlink:                       { yes - unlink our record               }
        MOV     EAX,[EAX]       { get next record on list               }
        MOV     [ECX],EAX       { unlink our record                     }
@@exit:
end;
{$IFEND}

{$ENDIF !PC_MAPPED_EXCEPTIONS} // not PC_MAPPED_EXCEPTIONS

var
  InitContext: TInitContext;
{$IFDEF MSWINDOWS}
  DLLThreadContext: TInitContext;
{$ENDIF}

type
  TProc = procedure;


{$IFDEF POSIX}
procedure CallProc(Proc: Pointer; GOT: Cardinal);
asm
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EBX
        MOV     EBX,EDX
        ADD     EAX,EBX
        CALL    EAX
        POP     EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
end;
{$ENDIF POSIX}

procedure FinalizeUnits;
var
  Count: Integer;
  Table: PUnitEntryTable;
  P: Pointer;
begin
  if InitContext.InitTable = nil then
    exit;
  Count := InitContext.InitCount;
  Table := InitContext.InitTable^.UnitInfo;
{$IFDEF POSIX}
  Inc(Cardinal(Table), InitContext.Module^.GOT);
{$ENDIF}
  try
    while Count > 0 do
    begin
      Dec(Count);
      InitContext.InitCount := Count;
      P := Table^[Count].FInit;
      if Assigned(P) and Assigned(Pointer(P^)) then
      begin
{$IFDEF POSIX}
        CallProc(P, InitContext.Module^.GOT);
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
        TProc(P)();
{$ENDIF}
      end;
    end;
  except
    FinalizeUnits;  { try to finalize the others }
    raise;
  end;
end;

const
  errCaption: array[0..5] of AnsiChar = ('E', 'r', 'r', 'o', 'r', #0);

{***********************************************************}
{$IFDEF TRIAL_EDITION}
{
    This code is used as part of the timeout test for
    applications built with trial editions of the product.  It provides
    the current local time in a format native to the platform in question.

    The linker will generate a checksum of _InitUnitPrep that it will
    place into linked units.  The code generated for _InitUnitPrep must
    not contain fixups actually in the image, as this could alter the
    code at load time, invalidating the checksum.  Take great care to
    make sure that this code is entirely position independent on all
    platforms and circumstances to avoid a serious problem!
}
{$IFDEF MSWINDOWS}
type
  TSystemTime = record
    wYear: Word;
    wMonth: Word;
    wDayOfWeek: Word;
    wDay: Word;
    wHour: Word;
    wMinute: Word;
    wSecond: Word;
    wMilliseconds: Word;
  end;
  TFileTime = record
        LowTime: Integer;
        HighTime: Integer;
  end;


procedure GetLocalTime(var lpSystemTime: TSystemTime); stdcall; external 'kernel32.dll' name 'GetLocalTime';
procedure SystemTimeToFileTime(const lpSystemTime: TSystemTime; var Dest: TFileTime); stdcall; external 'kernel32.dll' name 'SystemTimeToFileTime';

function _InitUnitPrep: Int64;
var
  SystemTime: TSystemTime;
  FileTime: TFileTime;
  Days: Int64;
begin
  GetLocalTime(SystemTime);
  SystemTimeToFileTime(SystemTime, FileTime);

    // used to hack the result to force a failure for testing:
  Days := 1000000000 div 100;
  Days := Days * 3600;
  Days := Days * 24;
  Days := Days * 31;
  Days := 0;

  Result := Int64(FileTime) + Days;
//  Dec(InitContext.InitTable^.UnitCount);
end;
{$ENDIF}
{$IFDEF LINUX}

function _InitUnitPrep: Integer;
var
  Days: Integer;
begin
  Days := 0;    // used to hack the result to force a failure for testing
    Result := _time(nil) + Days;
end;
{$ENDIF}

resourcestring
{$IFDEF LINUX}
  SExpiredMsg =
  'This module was compiled with a trial version of Kylix.'+#10+
  'The trial period has expired.'+#10;
{$ENDIF}
{$IFDEF MACOS}
  SExpiredMsg =
  'This module was compiled with a trial version of Delphi.'+#10+
  'The trial period has expired.'+#10;
{$ENDIF}
{$IFDEF MSWINDOWS}
  SExpiredMsg =
  'This module was compiled with a trial version of Delphi.'+#13+#10+
  'The trial period has expired.'+#13+#10;
{$ENDIF}
var
  ExpiredMsg: String;

procedure _Expired;
{$IFDEF MSWINDOWS}
var
  Dummy: Cardinal;
begin
  // U-OK
  ExpiredMsg := LoadResString(@SExpiredMsg);
  if IsConsole then
    WriteFile(GetStdHandle(STD_ERROR_HANDLE), PChar(ExpiredMsg), Length(ExpiredMsg), Dummy, nil)
  else
    MessageBox(0, PChar(ExpiredMsg), errCaption, 0);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  ExpiredMsg := LoadResString(@SExpiredMsg);
  __write(2, PChar(ExpiredMsg), Length(ExpiredMsg));
{$ENDIF POSIX}
  Halt(232);
end;

{$ENDIF} // TRIAL_EDITION

//function _printf(Format: PAnsiChar): Integer; cdecl; varargs;
//external libc name '_printf';

procedure InitUnits;
var
  Count, I: Integer;
  Table: PUnitEntryTable;
  P: Pointer;
begin
  if InitContext.InitTable = nil then
    exit;
  Count := InitContext.InitTable^.UnitCount;
  I := 0;
  Table := InitContext.InitTable^.UnitInfo;
{$IFDEF POSIX}
  Inc(Cardinal(Table), InitContext.Module^.GOT);
{$ENDIF}
  try
    while I < Count do
    begin
      P := Table^[I].Init;
      Inc(I);
      InitContext.InitCount := I;
      if Assigned(P) and Assigned(Pointer(P^)) then
      begin
{$IFDEF POSIX}
        CallProc(P, InitContext.Module^.GOT);
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
        TProc(P)();
{$ENDIF}
      end;
    end;
  except
    FinalizeUnits;
    raise;
  end;
end;

procedure _PackageLoad(const Table : PackageInfo; Module: PLibModule);
var
  SavedContext: TInitContext;
begin
  SavedContext := InitContext;
  InitContext.DLLInitState := 0;
  InitContext.InitTable := Table;
  InitContext.InitCount := 0;
  InitContext.Module := Module;
  InitContext.OuterContext := @SavedContext;
  try
    InitUnits;
  finally
    InitContext := SavedContext;
  end;
end;


procedure _PackageUnload(const Table : PackageInfo; Module: PLibModule);
var
  SavedContext: TInitContext;
begin
  SavedContext := InitContext;
  InitContext.DLLInitState := 0;
  InitContext.InitTable := Table;
  InitContext.InitCount := Table^.UnitCount;
  InitContext.Module := Module;
  InitContext.OuterContext := @SavedContext;
  try
    FinalizeUnits;
  finally
    InitContext := SavedContext;
  end;
end;

{$IF defined(LINUX) or defined(MACOS)}
procedure       _StartExe(InitTable: PackageInfo; Module: PLibModule; Argc: Integer; Argv: Pointer);
begin
  ArgCount := Argc;
  ArgValues := Argv;
{$IFEND LINUX or MACOS}
{$IFDEF MSWINDOWS}
procedure       _StartExe(InitTable: PackageInfo; Module: PLibModule);
begin
  RaiseExceptionProc := @RaiseException;
  RTLUnwindProc := @RTLUnwind;
{$ENDIF MSWINDOWS}
  InitContext.InitTable := InitTable;
  InitContext.InitCount := 0;
  InitContext.Module := Module;
  MainInstance := Module.Instance;
{$IFNDEF PC_MAPPED_EXCEPTIONS}
  SetExceptionHandler(@InitContext);
{$ENDIF !PC_MAPPED_EXCEPTIONS}
  IsLibrary := False;
  InitUnits;
end;

{$IFDEF MSWINDOWS}
{$IF not defined(CPU386)}


procedure _StartLib(InitTable: PackageInfo; Module: PLibModule; TlsProc: Pointer; DllProc: TDllProcEx; AHInst: HINST; Reason: LongWord; Reserved: Pointer);
type
  TlsProcType = array[0..3] of procedure;
  PTlsProcType = ^TlsProcType;
const
  DLL_PROCESS_DETACH = 0;
  DLL_PROCESS_ATTACH = 1;
  DLL_THREAD_ATTACH  = 2;
  DLL_THREAD_DETACH  = 3;
var
  Context: PInitContext;
  SavedContext: TInitContext;
begin
  Context := @InitContext;
  if Reason < DLL_THREAD_ATTACH then Context := @DllThreadContext;
  SavedContext := Context^;

  // Setup the current InitContext
  // Context.DLLSaveEBP
  // Context.DLLSaveEBX
  // Context.DLLSaveESI
  // Context.DLLSaveEDI
  Context.InitTable := InitTable;
  Context.Module := Module;
  Context.OuterContext := nil;
//        LEA     ECX,[EBP - (type TExcFrame) - (type TInitContext)]
//        MOV     [EBX].TInitContext.OuterContext,ECX

  // Get and save the current thread ID
  Context.ThreadID := GetCurrentThreadID;

  // Setup InitCount for FinalizeUnits call
  Context.InitTable.UnitCount := 0;
  if Reason = DLL_PROCESS_DETACH then
    Context.InitTable.UnitCount := Context.InitTable.UnitCount;

  // Setup exception handler
  RaiseExceptionProc := @RaiseException;
  RTLUnwindProc := @RTLUnwind;
  SetExceptionHandler(@Context);
  Context.DLLInitState := Reason + 1;

  // Init any needed TLS
  // call ExitThreadTLS proc after DLLProc
  if Reason < DLL_THREAD_DETACH then
    PTlsProcType(TlsProc)^[Reason]();

  // Call any DllProc
  if Assigned(DllProc) then
    DllProc(Reason, Reserved);

  // don't free TLS on process shutdown
  if Reason >= DLL_THREAD_DETACH then
    PTlsProcType(TlsProc)^[Reason]();

  // Set IsLibrary if there was no exe yet
  if MainInstance = 0 then
  begin
    IsLibrary := True;
    Default8087CW := Get8087CW;
  end;
  if Reason = DLL_PROCESS_ATTACH then
    InitUnits
  else
    _Halt0;
end;
{$ELSE CPU386}


procedure       _StartLib;
asm
        { ->    EAX InitTable   }
        {       EDX Module      }
        {       ECX InitTLS     }
        {       [ESP+4] DllProc }
        {       [EBP+8] HInst   }
        {       [EBP+12] Reason }
        {       [EBP-(typeTExcFrame)] TExcFrame local }
        {       [EBP-(type TExcFrame)-(type TInitContext)] TInitContext local }

        { Push some desperately needed registers }

        PUSH    ECX
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        { Setup EBX to point to InitContext or DLLThreadContext based on Reason }

        MOV     EBX,offset InitContext
        CMP     DWORD PTR [EBP+12],2    // DLL_THEAD_ATTACH
        JL      @@notDLLThread
        MOV     EBX,offset DLLThreadContext

        { Save the current init context into the stackframe of our caller }

@@notDLLThread:
        MOV     ESI,EBX
        LEA     EDI,[EBP - (type TExcFrame) - (type TInitContext)]
        MOV     ECX,(type TInitContext)/4
        REP     MOVSD

        { Setup the current InitContext }

        POP     [EBX].TInitContext.DLLSaveEDI
        POP     [EBX].TInitContext.DLLSaveESI
        POP     [EBX].TInitContext.DLLSaveEBX
        MOV     [EBX].TInitContext.DLLSaveEBP,EBP
        MOV     [EBX].TInitContext.InitTable,EAX
        MOV     [EBX].TInitContext.Module,EDX
        LEA     ECX,[EBP - (type TExcFrame) - (type TInitContext)]
        MOV     [EBX].TInitContext.OuterContext,ECX

        { Get and save the current thread ID }

        CALL    GetCurrentThreadID
        MOV     [EBX].TInitContext.ThreadID,EAX
        MOV     EAX,[EBX].TInitContext.InitTable

        { Setup InitCount for FinalizeUnits call }

        XOR     ECX,ECX
        CMP     DWORD PTR [EBP+12],0    // Reason = DLL_PROCESS_DETACH?
        JNE     @@notShutDown
        MOV     ECX,[EAX].PackageInfoTable.UnitCount
@@notShutDown:
        MOV     [EBX].TInitContext.InitCount,ECX

        { Setup exception handler }

        MOV     EAX, offset RaiseException
        MOV     RaiseExceptionProc, EAX
        MOV     EAX, offset RTLUnwind
        MOV     RTLUnwindProc, EAX

        MOV     EAX,EBX                 // Pass address of current context
        CALL    SetExceptionHandler

        MOV     EAX,[EBP+12]
        INC     EAX
        MOV     [EBX].TInitContext.DLLInitState,AL
        DEC     EAX

        { Init any needed TLS }

        POP     ECX
        MOV     EDX,[ECX]
        MOV     [EBX].TInitContext.ExitProcessTLS,EDX
        JE      @@skipTLSproc
        CMP     AL,3                    // DLL_THREAD_DETACH
        JGE     @@skipTLSproc           // call ExitThreadTLS proc after DLLProc
        CALL    dword ptr [ECX+EAX*4]   // Call TlsProc[Reason]

@@skipTLSproc:

        { Call any DllProc }

        PUSH    ECX                     // TlsProc
        MOV     ECX,[ESP+8]             // DLLProc
        TEST    ECX,ECX
        JE      @@noDllProc
        MOV     EAX,[EBP+12]            // Reason
        MOV     EDX,[EBP+16]            // Reserved
        CALL    ECX

@@noDllProc:

        POP     ECX
        MOV     EAX, [EBP+12]
        CMP     AL,3                    // DLL_THREAD_DETACH
        JL      @@afterDLLproc          // don't free TLS on process shutdown
        CALL    dword ptr [ECX+EAX*4]   // Call TlsProc[Reason]

@@afterDLLProc:

        { Set IsLibrary if there was no exe yet }

        CMP     MainInstance,0
        JNE     @@haveExe
        MOV     IsLibrary,1
        FNSTCW  Default8087CW           // save host exe's FPU preferences

@@haveExe:

        MOV     EAX,[EBP+12]
        DEC     EAX
        JNE     _Halt0
        CALL    InitUnits
        RET     4
end;
{$IFEND CPU386}
{$ENDIF  MSWINDOWS}
{$IFDEF POSIX}
procedure       _StartLib(Context: PInitContext; Module: PLibModule; DLLProc: TDLLProcEx);
var
  TempSwap: TInitContext;
begin
  // Context's register save fields are already initialized.
  // Save the current InitContext and activate the new Context by swapping them
  TempSwap := InitContext;
  InitContext := PInitContext(Context)^;
  PInitContext(Context)^ := TempSwap;

  InitContext.Module := Module;
  InitContext.OuterContext := Context;

  // DLLInitState is initialized by SysInit to 0 for shutdown, 1 for startup
  // Inc DLLInitState to distinguish from package init:
  // 0 for package, 1 for DLL shutdown, 2 for DLL startup

  Inc(InitContext.DLLInitState);

  if InitContext.DLLInitState = 1 then
  begin
    InitContext.InitTable := Module.InitTable;
    if Assigned(InitContext.InitTable) then
      InitContext.InitCount := InitContext.InitTable.UnitCount  // shutdown
  end
  else
  begin
    Module.InitTable := InitContext.InitTable;  // save for shutdown
    InitContext.InitCount := 0;  // startup
  end;

  if Assigned(DLLProc) then
    DLLProc(InitContext.DLLInitState-1,nil);

  if MainInstance = 0 then        { Set IsLibrary if there was no exe yet }
  begin
    IsLibrary := True;
    Default8087CW := Get8087CW;
  end;

  if InitContext.DLLInitState = 1 then
    _Halt0
  else
    InitUnits;
end;
{$ENDIF POSIX}

function LoadResStringA(ResStringRec: PResStringRec): AnsiString;
begin
  Result := AnsiString(LoadResString(ResStringRec));
end;

function LoadResStringW(ResStringRec: PResStringRec): WideString;
begin
  Result := WideString(LoadResString(ResStringRec));
end;

function LoadResStringU(ResStringRec: PResStringRec): UnicodeString;
begin
  Result := UnicodeString(LoadResString(ResStringRec));
end;




{$IF not defined(CPU386)}
procedure _InitResStrings(InitTable: _PResStringInitTable);
var
  I: Integer;
  P: _PResStringInitTableElem;
begin
  for I := 1 to InitTable^.Count do
  begin
    P := @InitTable^.Table[I];
    case P^.stringKind of
      _TResStringInitTableElem.LString:
        PAnsiString(P^.variableAddress)^ := LoadResStringA(P^.resStringAddress);
      _TResStringInitTableElem.WString:
        PWideString(P^.variableAddress)^ := LoadResStringW(P^.resStringAddress);
      _TResStringInitTableElem.UString:
        PUnicodeString(P^.variableAddress)^ := LoadResStringU(P^.resStringAddress);
    end;
  end;
end;
{$ELSE}
procedure _InitResStrings;
asm
        { ->    EAX     Pointer to init table               }
        {                 record                            }
        {                   cnt: Integer;                   }
        {                   tab: array [1..cnt] record      }
        {                      variableAddress: Pointer;    }
        {                      resStringAddress: Pointer;   }
        {                      stringKind: (LString, WString, UString) as Int32; }
        {                   end;                            }
        {                 end;                              }
        { EBX = caller's GOT for PIC callers, 0 for non-PIC }

{$IFDEF MSWINDOWS}
        PUSH    EBX
        XOR     EBX,EBX
{$ENDIF}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,[EBX+EAX]     // EDI := initTable.cnt
        LEA     ESI,[EBX+EAX+4]   // ESI := @initTable.tab
@@loop:
        MOV     EAX,[ESI+4]       // EAX := initTable.tab[i].resStringAddress
        ADD     EAX,EBX
        MOV     EDX,[ESI]         // EDX := initTable.tab[i].variableAddress
        ADD     EDX,EBX
        MOV     ECX,[ESI+8]       // ECX := initTable.tab[i].stringKind

        // Handle appropriate string kind.
        TEST    ECX,ECX
        JZ      @@lstring
        DEC     ECX
        JZ      @@wstring
        DEC     ECX
        JZ      @@ustring
        INT     3

@@lstring:
        CALL    LoadResStringA
        JMP     @@doneLoad

@@wstring:
        CALL    LoadResStringW
        JMP     @@doneLoad

@@ustring:
        CALL    LoadResStringU

@@doneLoad:
        ADD     ESI,12
        DEC     EDI
        JNZ     @@loop

        POP     ESI
        POP     EDI
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
{$IFDEF MSWINDOWS}
        POP     EBX
{$ENDIF}
end;
{$IFEND}




{$IF not defined(CPU386)}
procedure _InitResStringImports(InitTable: _PResStringImportInitTable);
var
  I: Integer;
  P: _PResStringImportInitTableElem;
begin
  for I := 1 to InitTable^.Count do
  begin
    P := @InitTable^.Table[I];
    case P^.stringKind of
      _TResStringImportInitTableElem.LString:
        PAnsiString(P^.variableAddress)^ := LoadResStringA(P^.resStringIndirAddress^);
      _TResStringImportInitTableElem.WString:
        PWideString(P^.variableAddress)^ := LoadResStringW(P^.resStringIndirAddress^);
      _TResStringImportInitTableElem.UString:
        PUnicodeString(P^.variableAddress)^ := LoadResStringU(P^.resStringIndirAddress^);
    end;
  end;
end;
{$ELSE}
procedure _InitResStringImports;
asm
        { ->    EAX     Pointer to init table               }
        {                 record                            }
        {                   cnt: Integer;                   }
        {                   tab: array [1..cnt] record      }
        {                      variableAddress: Pointer;    }
        {                      resStringAddress: ^Pointer; *** note indirection  }
        {                      stringKind: (LString, WString, UString) as Int32; }
        {                   end;                            }
        {                 end;                              }
        { EBX = caller's GOT for PIC callers, 0 for non-PIC }

{$IFDEF MSWINDOWS}
        PUSH    EBX
        XOR     EBX,EBX
{$ENDIF}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,[EBX+EAX]     // EDI := initTable.cnt
        LEA     ESI,[EBX+EAX+4]   // ESI := @initTable.tab
@@loop:
        MOV     EAX,[ESI+4]       // EAX := initTable.tab[i].resStringAddress
        MOV     EAX,[EBX+EAX]     // EAX := EAX^ (to do indirection)
        MOV     EDX,[ESI]         // EDX := initTable.tab[i].variableAddress
{$IFNDEF MACOS}
        ADD     EDX,EBX
{$ENDIF MACOS}
        MOV     ECX,[ESI+8]       // ECX := initTable.tab[i].stringKind

        // Handle appropriate string kind.
        TEST    ECX,ECX
        JZ      @@lstring
        DEC     ECX
        JZ      @@wstring
        DEC     ECX
        JZ      @@ustring
        INT     3

@@lstring:
        CALL    LoadResStringA
        JMP     @@doneLoad

@@wstring:
        CALL    LoadResStringW
        JMP     @@doneLoad

@@ustring:
        CALL    LoadResStringU

@@doneLoad:
        ADD     ESI,12
        DEC     EDI
        JNZ     @@loop

        POP     ESI
        POP     EDI
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
{$IFDEF MSWINDOWS}
        POP     EBX
{$ENDIF}
end;
{$IFEND}




{$IF not defined(CPU386)}
procedure _InitImports(InitTable: _PImportInitTable);
var
  I: Integer;
  P: _PImportInitTableElem;
begin
  for I := 1 to InitTable^.Count do
  begin
    P := @InitTable^.Table[I];
    PPointer(P^.variableAddress)^ := Pointer(NativeInt(P^.sourceIndirAddress^) + P^.soruceOffset);
  end;
end;
{$ELSE}
procedure _InitImports;
asm
        { ->    EAX     Pointer to init table               }
        {                 record                            }
        {                   cnt: Integer;                   }
        {                   tab: array [1..cnt] record      }
        {                      variableAddress: Pointer;    }
        {                      sourceAddress: ^Pointer;     }
        {                      sourceOffset: Longint;       }
        {                   end;                            }
        {                 end;                              }
        { ->    EDX     Linux only, this points to          }
        {               SysInit.ModuleIsCpp                 }
        { EBX = caller's GOT for PIC callers, 0 for non-PIC }
{$IFDEF MACOS}

	RET
{$ENDIF MACOS}
{$IFDEF MSWINDOWS}
        PUSH    EBX
        XOR     EBX,EBX
{$ENDIF MSWINDOWS}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EDI
        PUSH    ESI
        MOV     EDI,[EBX+EAX]
        LEA     ESI,[EBX+EAX+4]
{$IFDEF LINUX}
        {
            The C++ linker may have already fixed these things up to valid
            addresses.  In this case, we don't want to do this pass.  If this
            module's init tab was linked with ilink, then SysInit.ModuleIsCpp
            will be set, and we'll bail out.
        }
        CMP     BYTE PTR[EDX+EBX], 0  { SysInit.ModuleIsCpp }
        JNE     @@exit
{$ENDIF LINUX}
@@loop:
        MOV     EAX,[ESI+4]     { load address of import    }
        MOV     EDX,[ESI]       { load address of variable  }
        MOV     EAX,[EBX+EAX]   { load contents of import   }
        ADD     EAX,[ESI+8]     { calc address of variable  }
        MOV     [EBX+EDX],EAX   { store result              }
        ADD     ESI,12
        DEC     EDI
        JNZ     @@loop

@@exit:

        POP     ESI
        POP     EDI
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
{$IFDEF MSWINDOWS}
        POP     EBX
{$ENDIF MSWINDOWS}
end;
{$IFEND}



{$IFDEF MSWINDOWS}
{$IF not defined(CPU386)}
procedure _InitWideStrings(InitTable: _PWideStringInitTable);
var
  I: Integer;
  P: _PWideStringInitTableElem;
begin
  for I := 1 to InitTable^.Count do
  begin
    P := @InitTable^.Table[I];
    _WStrAsg(PWideString(P^.variableAddress)^, WideString(P^.stringAddress));
  end;
end;
{$ELSE CPU386}
procedure _InitWideStrings;
asm
     { ->    EAX     Pointer to init table               }
     {                 record                            }
     {                   cnt: Integer;                   }
     {                   tab: array [1..cnt] record      }
     {                      variableAddress: Pointer;    }
     {                      stringAddress: ^Pointer;     }
     {                   end;                            }
     {                 end;                              }

    PUSH    EBX
    PUSH    ESI
    MOV     EBX,[EAX]
    LEA     ESI,[EAX+4]
@@loop:
    MOV     EDX,[ESI+4]     { load address of string    }
    MOV     EAX,[ESI]       { load address of variable  }
    CALL    _WStrAsg
    ADD     ESI,8
    DEC     EBX
    JNZ     @@loop

    POP     ESI
    POP     EBX
end;
{$IFEND CPU386}
{$ENDIF MSWINDOWS}

{$IF defined(CPUX64)}
var
  runErrMsg: array[0..37] of AnsiChar = (
    'R', 'u', 'n', 't', 'i', 'm', 'e', ' ', // 0..7
    'e', 'r', 'r', 'o', 'r', ' ', ' ', ' ', // 8..15
    ' ', ' ', 'a', 't', ' ', '0', '0', '0', // 16..23
    '0', '0', '0', '0', '0', '0', '0', '0', // 24..31
    '0', '0', '0', '0', '0', #0);           // 32..37
{$ELSE}
var
  runErrMsg: array[0..29] of AnsiChar = (
    'R', 'u', 'n', 't', 'i', 'm', 'e', ' ', // 0..7
    'e', 'r', 'r', 'o', 'r', ' ', ' ', ' ', // 8..15
    ' ', ' ', 'a', 't', ' ', '0', '0', '0', // 16..23
    '0', '0', '0', '0', '0', #0);           // 24..29
{$IFEND}

const
  hexDigits: array[0..15] of AnsiChar = (
    '0', '1', '2', '3', '4', '5', '6', '7',
    '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');

procedure MakeErrorMessage;
var
  digit: Byte;
  Temp: Integer;
  Addr: NativeUInt;
begin
  digit := 16;
  Temp := ExitCode;
  repeat
    runErrMsg[digit] := AnsiChar(Ord('0') + (Temp mod 10));
    Temp := Temp div 10;
    Dec(digit);
  until Temp = 0;
{$IF defined(CPUX64)}
  digit := 36;
{$ELSE}
  digit := 28;
{$IFEND}
  Addr := NativeUInt(ErrorAddr);
  repeat
    runErrMsg[digit] := hexDigits[Addr and $F];
    Addr := Addr div 16;
    Dec(digit);
  until Addr = 0;
end;







procedure ExitDll(Context: PInitContext);
{$IF not defined(CPU386)}
begin
end;
{$ELSE}
asm
        { ->    EAX  PInitContext }

        { Restore the InitContext }
        MOV     EDI,EAX
        MOV     EBX,[EDI].TInitContext.DLLSaveEBX
        MOV     EBP,[EDI].TInitContext.DLLSaveEBP
        PUSH    [EDI].TInitContext.DLLSaveESI
        PUSH    [EDI].TInitContext.DLLSaveEDI

        MOV     ESI,[EDI].TInitContext.OuterContext
        MOV     ECX,(type TInitContext)/4
        REP     MOVSD
        POP     EDI
        POP     ESI

{$IFDEF MSWINDOWS}
        // Linux: See notes in legacy versions of this file.
        { Return False if ExitCode <> 0, and set ExitCode to 0 }
        XOR     EAX,EAX
        XCHG    EAX, ExitCode
        NEG     EAX
        SBB     EAX,EAX
        INC     EAX
{$ENDIF MSWINDOWS}

        LEAVE
{$IFDEF MSWINDOWS}
        RET     12
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
        RET
{$ENDIF LINUX}
end;
{$IFEND}

procedure WriteErrorMessage;
{$IFDEF MSWINDOWS}
var
  Dummy: Cardinal;
begin
  if IsConsole then
  begin
    with TTextRec(Output) do
    begin
      if (Mode = fmOutput) and (BufPos > 0) then
        TTextIOFunc(InOutFunc)(TTextRec(Output));  // flush out text buffer
    end;
    // Leave #0 off end of runErrMsg
    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), runErrMsg, Sizeof(runErrMsg) - 1, Dummy, nil);
    WriteFile(GetStdHandle(STD_OUTPUT_HANDLE), sLineBreak, 2, Dummy, nil);
  end
  else if not NoErrMsg then
    MessageBoxA(0, runErrMsg, errCaption, 0);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
var
  c: AnsiChar;
begin
  with TTextRec(Output) do
  begin
    if (Mode = fmOutput) and (BufPos > 0) then
      TTextIOFunc(InOutFunc)(TTextRec(Output));  // flush out text buffer
  end;
   __write(STDERR_FILENO, @runErrMsg, Sizeof(runErrMsg)-1);
   c := sLineBreak;
   __write(STDERR_FILENO, @c, 1);
{$ENDIF POSIX}
end;

var
  RTLInitFailed: Boolean = False;

procedure _Halt0;
var
  P: procedure;

{$IFNDEF PC_MAPPED_EXCEPTIONS}
  ExceptObject: TObject;
{$ENDIF PC_MAPPED_EXCEPTIONS}
begin
{$IF defined(LINUX) or defined(MACOS)}
  if (ExitCode <> 0) and CoreDumpEnabled then
    __raise(SIGABRT);

  if (InitContext.DLLInitState = 2) and (ExitCode <> 0) then
    RTLInitFailed := True;

  if (InitContext.DLLInitState = 1) and RTLInitFailed then
    // RTL failed to initialized in library startup.  Units have already been
    // finalized, don't finalize them again.
    ExitDll(@InitContext);
{$IFEND LINUX or MACOS}

  { If there was some kind of runtime error, alert the user }

  if ErrorAddr <> nil then
  begin
    MakeErrorMessage;
    WriteErrorMessage;
    ErrorAddr := nil;
  end;

  { For DLL_THREAD_ATTACH or DLL_THREAD_DETACH, just cleanup and exit }

{$IFDEF MSWINDOWS}
  if Assigned(DLLThreadContext.ExcFrame) and
    (GetCurrentThreadId = DLLThreadContext.ThreadID) then
  begin
{$IFNDEF PC_MAPPED_EXCEPTIONS}
    UnsetExceptionHandler(@DLLThreadContext);
{$ENDIF !PC_MAPPED_EXCEPTIONS}
    ExitDll(@DLLThreadContext);
  end;
{$ENDIF MSWINDOWS}

  if InitContext.DLLInitState = 0 then
    while ExitProc <> nil do
    begin
      @P := ExitProc;
      ExitProc := nil;
      P;
    end;

  { This loop exists because we might be nested in PackageLoad calls when }
  { Halt got called. We need to unwind these contexts.                    }

  while True do
  begin

    { If we are a library, and we are starting up fine, there are no units to finalize }

    if (InitContext.DLLInitState = 2) and (ExitCode = 0) then
      InitContext.InitCount := 0;

    { Clear the exception stack to prevent handled exceptions from being shown to the user }
{$IFNDEF PC_MAPPED_EXCEPTIONS}








    ExceptObject := TObject(AcquireExceptionObject);
    while ExceptObject <> nil do
    begin
      ExceptObject.Free;
      ExceptObject := TObject(AcquireExceptionObject);
    end;
{$ENDIF PC_MAPPED_EXCEPTIONS}

    { Undo any unit initializations accomplished so far }

    FinalizeUnits;

    if (InitContext.DLLInitState <= 1) or (ExitCode <> 0) then
    begin
      if InitContext.Module <> nil then
        with InitContext do
        begin
          UnregisterModule(Module);
{$IFDEF PC_MAPPED_EXCEPTIONS}
          SysUnregisterIPLookup(Module.CodeSegStart);
{$ENDIF PC_MAPPED_EXCEPTIONS}
          if (Module.ResInstance <> Module.Instance) and (Module.ResInstance <> 0) then
{$IFDEF MSWINDOWS}
            FreeLibrary(Module.ResInstance);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
            dlclose(Module.ResInstance);
{$ENDIF POSIX}
        end;
    end;

{$IFNDEF PC_MAPPED_EXCEPTIONS}
    UnsetExceptionHandler(@InitContext);
{$ENDIF PC_MAPPED_EXCEPTIONS}

{$IFDEF MSWINDOWS}
    if InitContext.DllInitState = 1 then
      InitContext.ExitProcessTLS;
{$ENDIF MSWINDOWS}

    if InitContext.DllInitState <> 0 then
      ExitDll(@InitContext);

    if InitContext.OuterContext = nil then
    begin
      {
        If an ExitProcessProc is set, we call it.  Note that at this
        point the RTL is completely shutdown.  The only thing this is used
        for right now is the proper semantic handling of signals under Linux.
      }
      if Assigned(ExitProcessProc) then
        ExitProcessProc;
{$IFDEF MSWINDOWS}
      ExitProcess(ExitCode);
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
      __exit(ExitCode);
{$ENDIF POSIX}
    end;

    InitContext := InitContext.OuterContext^
  end;
end;

procedure _Halt;
begin
  ExitCode := Code;
  _Halt0;
end;




procedure _Run0Error;
{$IFDEF PUREPASCAL}
begin
  _RunError(0);   // loses return address
end;
{$ELSE}
asm
        XOR     EAX,EAX
        JMP     _RunError
end;
{$ENDIF}





procedure _RunError(errorCode: Byte);
{$IFDEF PUREPASCAL}
begin
  ErrorAddr := Pointer(-1);  // no return address available
  Halt(errorCode);
end;
{$ELSE !PUREPASCAL}
asm
{$IFDEF PIC}
        PUSH    EAX
        CALL    GetGOT
        MOV     EBX, EAX
        POP     EAX
        MOV     ECX, [EBX].ErrorAddr
        POP     [ECX]
{$ELSE}
        POP     ErrorAddr
{$ENDIF}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        JMP     _Halt
end;
{$ENDIF !PUREPASCAL}

{$IFDEF PC_MAPPED_EXCEPTIONS}
procedure _UnhandledException;
type TExceptProc = procedure (Obj: TObject; Addr: Pointer);
begin
  if Assigned(ExceptProc) then
    TExceptProc(ExceptProc)(ExceptObject, ExceptAddr)
  else
    RunErrorAt(230, ExceptAddr);
end;
{$ENDIF PC_MAPPED_EXCEPTIONS}






{$IFOPT O+}
  // Turn off optimizations to force creating a EBP stack frame and
  // place locals on the stack.  Because the var ReturnAddr is on
  // the stack in a known location, we can find the return address
  // relative to it.
  {$DEFINE OPTIMIZATIONSON}
  {$O-}
{$ENDIF}
procedure _Assert(const Message, Filename: string; LineNumber: Integer);
var
  ReturnAddr: Pointer;
begin
  // Stack looks like this on x86:
  // [EBP + 4]   : Return Address
  // [EBP]       : Stack frame
  // [EBP - 4]   : Message : string
  // [EBP - 8]   : Filename : string
  // [EBP - 12]  : LineNumber : Integer
  // [EBP - 16]  : ReturnAddr : Pointer
  ReturnAddr := PPointer(Integer(@ReturnAddr) + SizeOf(string) * 2 +
    SizeOf(Integer) + SizeOf(Pointer) * 2)^;
  if Assigned(AssertErrorProc) then
    AssertErrorProc(Message, Filename, LineNumber, ReturnAddr)
  else
    ErrorAt(Byte(reAssertionFailed), ReturnAddr);
end;
{$IFDEF OPTIMIZATIONSON}
  {$UNDEF OPTIMIZATIONSON}
  {$O+}
{$ENDIF}

type
  PThreadRec = ^TThreadRec;
  TThreadRec = record
    {
      WARNING: Don't change these fields without also changing them in
      the C++ RTL : winrtl/source/vcl/crtlvcl.cpp
    }
    Func: TThreadFunc;
    Parameter: Pointer;
  end;





{$IFDEF MSWINDOWS}
function ThreadWrapper(Parameter: Pointer): Integer; stdcall;
{$ELSE}
function ThreadWrapper(Parameter: Pointer): Pointer; cdecl;
{$ENDIF}
{$IF not defined(CPU386)}
begin
end;
{$ELSE}
asm
{$IFDEF PC_MAPPED_EXCEPTIONS}
        { Mark the top of the stack with a signature }
        PUSH    UNWINDFI_TOPOFSTACK
{$ENDIF PC_MAPPED_EXCEPTIONS}
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    _FpuInit
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EBP
{$IFNDEF PC_MAPPED_EXCEPTIONS}
        XOR     ECX,ECX
        PUSH    offset _ExceptionHandler
        MOV     EDX,FS:[ECX]
        PUSH    EDX
        MOV     FS:[ECX],ESP
{$ENDIF !PC_MAPPED_EXCEPTIONS}
{$IFDEF PC_MAPPED_EXCEPTIONS}
    // The signal handling code in SysUtils depends on being able to
    // discriminate between Delphi threads and foreign threads in order
    // to choose the disposition of certain signals.  It does this by
    // testing a TLS index.  However, we allocate TLS in a lazy fashion,
    // so this test can fail unless we've already allocated the TLS segment.
    // So we force the allocation of the TLS index value by touching a TLS
    // value here.  So don't remove this silly call to AreOSExceptionsBlocked.
        CALL    AreOSExceptionsBlocked
{$ENDIF PC_MAPPED_EXCEPTIONS}
        MOV     EAX,Parameter

        MOV     ECX,[EAX].TThreadRec.Parameter
        MOV     EDX,[EAX].TThreadRec.Func
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    ECX
        PUSH    EDX
        CALL    _FreeMem
        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        CALL    EDX

{$IFNDEF PC_MAPPED_EXCEPTIONS}
        XOR     EDX,EDX
        POP     ECX
        MOV     FS:[EDX],ECX
        POP     ECX
{$ENDIF !PC_MAPPED_EXCEPTIONS}
        POP     EBP
{$IFDEF PC_MAPPED_EXCEPTIONS}
        { Ditch our TOS marker }
        ADD     ESP, 4
{$ENDIF PC_MAPPED_EXCEPTIONS}
end;
{$IFEND}


{$IFDEF MSWINDOWS}
function BeginThread(SecurityAttributes: Pointer; StackSize: LongWord;
  ThreadFunc: TThreadFunc; Parameter: Pointer; CreationFlags: LongWord;
  var ThreadId: TThreadID): Integer;
var
  P: PThreadRec;
begin
  if Assigned(SystemThreadFuncProc) then
    P := PThreadRec(SystemThreadFuncProc(ThreadFunc, Parameter))
  else
  begin
    New(P);
    P.Func := ThreadFunc;
    P.Parameter := Parameter;
  end;

  IsMultiThread := TRUE;

  Result := CreateThread(SecurityAttributes, StackSize, @ThreadWrapper, P,
    CreationFlags, ThreadID);

  { P variable is supposed to be freed by the ThreadWrapper routine.
    If the call to CreateThread fails, then ThreadWrapper will not be called
    and P will not get freed. Check for failure now and free P if required.
  }
  if Result = 0 then
    Dispose(P);
end;


procedure EndThread(ExitCode: Integer);
begin
  if Assigned(SystemThreadEndProc) then
    SystemThreadEndProc(ExitCode);
  ExitThread(ExitCode);
end;
{$ENDIF}

{$IFDEF POSIX}
function BeginThread(Attribute: PThreadAttr;
                     ThreadFunc: TThreadFunc;
                     Parameter: Pointer;
                     var ThreadId: TThreadID): Integer;
var
  P: PThreadRec;
begin
  if Assigned(BeginThreadProc) then
    Result := BeginThreadProc(Attribute, ThreadFunc, Parameter, ThreadId)
  else
  begin
    New(P);
    P.Func := ThreadFunc;
    P.Parameter := Parameter;
    IsMultiThread := True;

    Result := pthread_create(pthread_t(ThreadID), Ppthread_attr_t(Attribute), @ThreadWrapper, P);

    { P variable is supposed to be freed by the ThreadWrapper routine.
      If the call to CreateThread fails, then ThreadWrapper will not be called
      and P will not get freed. Check for failure now and free P if required.
    }
    if Result <> 0 then
      Dispose(P);
  end;
end;

procedure EndThread(ExitCode: Integer);
begin
  if Assigned(EndThreadProc) then
    EndThreadProc(ExitCode);
  // No "else" required since EndThreadProc does not (!!should not!!) return.
  pthread_detach(pthread_t(GetCurrentThreadID));
  pthread_exit(ExitCode);
end;
{$ENDIF POSIX}


{ STRING SUPPORT }

{ ----------------------------------------------------- }
{       internal functions & procedures for strings     }
{ ----------------------------------------------------- }

// S must be non-nil.
function __StringLength(const S: UnicodeString): LongInt; overload; inline;
begin
  Result := PLongInt(NativeInt(S) - 4)^;                // StrRec.length
end;

// S must be non-nil.
// Returns number of characters.
// Note: On Windows, length field contains number of bytes and not number
//       of characters.
function __StringLength(const S: WideString): LongInt; overload; inline;
begin
{$IFDEF MSWINDOWS}
  Result := PLongInt(NativeInt(S) - 4)^ div 2;          // size field of BSTR
{$ELSE}
  Result := PLongInt(NativeInt(S) - 4)^;                // StrRec.length
{$ENDIF}
end;

// S must be non-nil
function __StringLength(const S: RawByteString): LongInt; overload; inline;
begin
  Result := PLongInt(NativeInt(S) - 4)^;                // StrRec.length
end;

// S must be non-nil
function __StringLength(const S: Pointer): LongInt; overload; inline;
begin
  Result := PLongInt(NativeInt(S) - 4)^;                // StrRec.length
end;

// S must be non-nil
function __StringRefCnt(const S: UnicodeString): LongInt; overload; inline;
begin
  Result := PLongInt(NativeInt(S) - 8)^;                // StrRec.refCnt
end;

{$IFNDEF MSWINDOWS}
// S must be non-nil
// Note: On Windows, WideString doesn't contain refCount field.
function __StringRefCnt(const S: WideString): LongInt; overload; inline;
begin
  Result := PLongInt(NativeInt(S) - 8)^;                // StrRec.refCnt
end;
{$ENDIF}

// S must be non-nil
function __StringRefCnt(const S: RawByteString): LongInt; overload; inline;
begin
  Result := PLongInt(NativeInt(S) - 8)^;                // StrRec.refCnt
end;

// S must be non-nil. Don't use for Windows WideString.
function __StringRefCnt(const S: Pointer): LongInt; overload; inline;
begin
  Result := PLongInt(NativeInt(S) - 8)^;                // StrRec.refCnt
end;

// S must be non-nil
function __StringCodePage(const S: UnicodeString): Word; overload; inline;
begin
  Result := PWord(NativeInt(S) - 12)^;                  // StrRec.codePage
end;

{$IFNDEF MSWINDOWS}
// S must be non-nil
// Note: On Windows, WideString doesn't contain codePage field.
function __StringCodePage(const S: WideString): Word; overload; inline;
begin
  Result := PWord(NativeInt(S) - 12)^;                  // StrRec.codePage
end;
{$ENDIF}

// S must be non-nil
function __StringCodePage(const S: RawByteString): Word; overload; inline;
begin
  Result := PWord(NativeInt(S) - 12)^;                  // StrRec.codePage
end;

// S must be non-nil. Don't use for Windows WideString.
function __StringCodePage(const S: Pointer): Word; overload; inline;
begin
  Result := PWord(NativeInt(S) - 12)^;                  // StrRec.codePage
end;


{ ------------------------------------------------------------- }
{       Compiler helper for string allocation and release       }
{ ------------------------------------------------------------- }





function _NewUnicodeString(CharLength: LongInt): Pointer;
{$IFDEF PUREPASCAL}
var
  P: PStrRec;
begin
  Result := nil;
  if CharLength > 0 then
  begin
    // Allocate a memory with record and extra wide-null terminator.
    if CharLength >= (MaxInt - SizeOf(StrRec)) div SizeOf(WideChar) then _IntOver;
    GetMem(P, SizeOf(StrRec) + (CharLength + 1) * SizeOf(WideChar));
    Result := Pointer(NativeInt(P) + SizeOf(StrRec));
    P.length := CharLength;
    P.refCnt := 1;
    P.elemSize := SizeOf(WideChar);
    P.codePage := Word(DefaultUnicodeCodePage);
    PWideChar(Result)[CharLength] := #0;
  end;
end;
{$ELSE}
asm
        { ->    EAX     length                  }
        { <-    EAX     pointer to new string   }
        TEST    EAX,EAX
        JLE     @@lengthLEZero  // length <= 0?
        PUSH    EAX             // save length
        ADD     EAX,EAX         // convert to bytes
        JO      @@overflow
        ADD     EAX,rOff+2      // + record + terminator
        JO      @@overflow
        {$IFDEF ALIGN_STACK}
        SUB     ESP,8
        {$ENDIF ALIGN_STACK}
        CALL    _GetMem
        {$IFDEF ALIGN_STACK}
        ADD     ESP,8
        {$ENDIF ALIGN_STACK}
        ADD     EAX,rOff
        POP     EDX                              // requested string length
        MOV     [EAX-skew].StrRec.refCnt,1
        MOV     [EAX-skew].StrRec.length,EDX
        MOV     word ptr [EAX+EDX*2],0           // wide null terminator
        MOV     word ptr [EAX-skew].StrRec.elemSize,2
{$IFDEF PIC}
        PUSH    EBX
        PUSH    EAX
        PUSH    ECX
        CALL    GetGOT
        MOV     EDX, [EAX].OFFSET DefaultUnicodeCodePage
        MOV     EDX, [EDX]
        POP     ECX
        POP     EAX
        POP     EBX
{$ELSE !PIC}
        MOV     EDX, DefaultUnicodeCodePage
{$ENDIF}
        MOV     word ptr [EAX-skew].StrRec.codePage,DX
        RET
@@overflow:
        {$IFDEF ALIGN_STACK}
        POP     EAX
        {$ENDIF ALIGN_STACK}
        JMP     _IntOver
@@lengthLEZero:
        XOR     EAX,EAX
end;
{$ENDIF !PUREPASCAL}






function _NewAnsiString(CharLength: LongInt; CodePage: Word): Pointer;
{$IFDEF PUREPASCAL}
var
  P: PStrRec;
begin
  Result := nil;
  if CharLength > 0 then
  begin
    // Alloc an extra null for strings with even length.  This has no actual
    // cost since the allocator will round up the request to an even size
    // anyway. All widestring allocations have even length, and need a double
    // null terminator.
    if CharLength >= MaxInt - SizeOf(StrRec) then _IntOver;
    GetMem(P, CharLength + SizeOf(StrRec) + 1 + ((CharLength + 1) and 1));
    Result := Pointer(NativeInt(P) + SizeOf(StrRec));
    P.length := CharLength;
    P.refcnt := 1;
    if CodePage = 0 then
      CodePage := Word(DefaultSystemCodePage);
    P.codePage := CodePage;
    P.elemSize := 1;
    PWideChar(Result)[CharLength div 2] := #0;  // length guaranteed >= 2
  end;
end;
{$ELSE}
asm
        { ->    EAX     length                  }
        { <-    EAX pointer to new string       }

        TEST    EAX,EAX
        JLE     @@lengthLEZero
        PUSH    EAX
        ADD     EAX,rOff+2                      // one or two nulls (Ansi/Wide)
        JO      @@overflow
        AND     EAX, not 1                      // round up to even length
        PUSH    EDX
        PUSH    EAX
        CALL    _GetMem
        POP     EDX                             // actual allocated length (>= 2)
        POP     ECX
        MOV     word ptr [EAX+EDX-2],0          // double null terminator
        ADD     EAX,rOff
        POP     EDX                             // requested string length
        MOV     [EAX-skew].StrRec.length,EDX
        MOV     [EAX-skew].StrRec.refCnt,1
        TEST    ECX,ECX
        JNE     @@NotDefault
{$IFDEF PIC}
        PUSH    EBX
        PUSH    EAX
        CALL    GetGOT
        MOV     ECX,[EAX].OFFSET DefaultSystemCodePage
        MOV     ECX, [ECX]
        POP     EAX
        POP     EBX
{$ELSE !PIC}
        MOV     ECX,DefaultSystemCodePage
{$ENDIF !PIC}
@@NotDefault:
        MOV     EDX,ECX
        MOV     word ptr [EAX-skew].StrRec.codePage,DX
        MOV     word ptr [EAX-skew].StrRec.elemSize,1
        RET
@@overflow:
        {$IFDEF ALIGN_STACK}
        POP     EAX
        {$ENDIF ALIGN_STACK}
        JMP     _IntOver
@@lengthLEZero:
        XOR     EAX,EAX
end;
{$ENDIF !PUREPASCAL}




{$IFDEF MSWINDOWS}
procedure WStrError;
{$IF not defined(CPU386)}
begin
  Error(reOutOfMemory);
end;
{$ELSE}
asm
        MOV     AL,reOutOfMemory
        JMP     Error
end;
{$IFEND}
{$ENDIF MSWINDOWS}



function _NewWideString(CharLength: LongInt): Pointer;
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
   Result := _NewUnicodeString(CharLength);
end;
{$ELSE}
asm
        JMP     _NewUnicodeString
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
{$IF not defined(CPU386)}
begin
  Result := nil;
  if CharLength <> 0 then
  begin
    Result := SysAllocStringLen(nil, CharLength);
    if Result = nil then
      WStrError;
  end;
end;
{$ELSE}
asm
        { ->    EAX     length                  }
        { <-    EAX     pointer to new string   }

        TEST    EAX,EAX
        JE      @@1
        {$IFDEF ALIGN_STACK}
        SUB     ESP,4
        {$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    0
        CALL    SysAllocStringLen
        {$IFDEF ALIGN_STACK}
        ADD     ESP,4
        {$ENDIF ALIGN_STACK}
        TEST    EAX,EAX
        JE      WStrError
@@1:
end;
{$IFEND}
{$ENDIF}



{$IFDEF PUREPASCAL}
function _UStrClr(var S): Pointer;
var
  P: PStrRec;
begin
  if Pointer(S) <> nil then
  begin
    P := Pointer(NativeInt(S) - SizeOf(StrRec));
    Pointer(S) := nil;
    if P.refCnt > 0 then
    begin
      if InterlockedDecrement(P.refCnt) = 0 then
        FreeMem(P);
    end;
  end;
  Result := @S;
end;
{$ELSE}
procedure _UStrClr(var S);
asm
        { ->    EAX     pointer to str  }
        { <-    EAX     pointer to str  }

        MOV     EDX,[EAX]                       { fetch str                     }
        TEST    EDX,EDX                         { if nil, nothing to do         }
        JE      @@done
        MOV     dword ptr [EAX],0               { clear str                     }
        MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                  }
        DEC     ECX                             { if < 0: literal str           }
        JL      @@done
   LOCK DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount       }
        JNE     @@done
        {$IFDEF ALIGN_STACK}
        SUB     ESP,8
        {$ENDIF ALIGN_STACK}
        PUSH    EAX
        LEA     EAX,[EDX-skew]                  { if refCnt now zero, deallocate}
        CALL    _FreeMem
        POP     EAX
        {$IFDEF ALIGN_STACK}
        ADD     ESP,8
        {$ENDIF ALIGN_STACK}
@@done:
end;
{$ENDIF !PUREPASCAL}



{$IFDEF PUREPASCAL}
function _LStrClr(var S): Pointer;
var
  P: PStrRec;
begin
  if Pointer(S) <> nil then
  begin
    P := Pointer(NativeInt(S) - SizeOf(StrRec));
    Pointer(S) := nil;
    if P.refCnt > 0 then
    begin
      if InterlockedDecrement(P.refCnt) = 0 then
        FreeMem(P);
    end;
  end;
  Result := @S;
end;
{$ELSE}
procedure _LStrClr(var S);
asm
        { ->    EAX     pointer to str  }
        { <-    EAX     pointer to str  }

        MOV     EDX,[EAX]                       { fetch str                     }
        TEST    EDX,EDX                         { if nil, nothing to do         }
        JE      @@done
        MOV     dword ptr [EAX],0               { clear str                     }
        MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                  }
        DEC     ECX                             { if < 0: literal str           }
        JL      @@done
   LOCK DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount       }
        JNE     @@done
        {$IFDEF ALIGN_STACK}
        SUB     ESP,8
        {$ENDIF ALIGN_STACK}
        PUSH    EAX
        LEA     EAX,[EDX-skew]                  { if refCnt now zero, deallocate}
        CALL    _FreeMem
        POP     EAX
        {$IFDEF ALIGN_STACK}
        ADD     ESP,8
        {$ENDIF ALIGN_STACK}
@@done:
end;
{$ENDIF !PUREPASCAL}



{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
function _WStrClr(var S): Pointer;
begin
  Result := _UStrClr(S);
end;
{$ELSE}
procedure _WStrClr(var S);
asm
        JMP     _UStrClr;
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
function _WStrClr(var S): Pointer;
var
  P: Pointer;
begin
  if Pointer(S) <> nil then
  begin
    P := Pointer(S);
    Pointer(S) := nil;
    SysFreeString(WideString(P));
  end;
  Result := @S;
end;
{$ELSE}
procedure _WStrClr(var S);
asm
        { ->    EAX     pointer to str  }
        { <-    EAX     pointer to Str  }

        MOV     EDX,[EAX]
        TEST    EDX,EDX
        JE      @@1
        MOV     DWORD PTR [EAX],0
        {$IFDEF ALIGN_STACK}
        SUB     ESP,4
        {$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        CALL    SysFreeString
        POP     EAX
        {$IFDEF ALIGN_STACK}
        ADD     ESP,4
        {$ENDIF ALIGN_STACK}
@@1:
end;
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}





procedure _UStrArrayClr(var StrArray; Count: Integer);
{$IF not defined(CPU386)}
var
  P: Pointer;
begin
  P := @StrArray;
  while Count > 0 do
  begin
    _UStrClr(P^);
    Dec(Count);
    Inc(NativeInt(P), SizeOf(Pointer));
  end;
end;
{$ELSE}
asm
        { ->    EAX pointer to str      }
        {       EDX cnt                 }

        {$IFDEF ALIGN_STACK}
        SUB     ESP,4
        {$ENDIF ALIGN_STACK}
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
        MOV     ESI,EDX

@@loop:
        MOV     EDX,[EBX]                       { fetch str                     }
        TEST    EDX,EDX                         { if nil, nothing to do         }
        JE      @@doneEntry
        MOV     dword ptr [EBX],0               { clear str                     }
        MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                  }
        DEC     ECX                             { if < 0: literal str           }
        JL      @@doneEntry
   LOCK DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount       }
        JNE     @@doneEntry
        LEA     EAX,[EDX-skew]                  { if refCnt now zero, deallocate}
        CALL    _FreeMem
@@doneEntry:
        ADD     EBX,4
        DEC     ESI
        JNE     @@loop

        POP     ESI
        POP     EBX
        {$IFDEF ALIGN_STACK}
        ADD     ESP,4
        {$ENDIF ALIGN_STACK}
end;
{$IFEND}





procedure _LStrArrayClr(var StrArray; cnt: longint);
{$IFDEF PUREPASCAL}
var
  P: Pointer;
begin
  P := @StrArray;
  while cnt > 0 do
  begin
    _LStrClr(P^);
    Dec(cnt);
    Inc(NativeInt(P), SizeOf(Pointer));
  end;
end;
{$ELSE}
asm
        { ->    EAX pointer to str      }
        {       EDX cnt                 }

        {$IFDEF ALIGN_STACK}
        SUB     ESP,4
        {$ENDIF ALIGN_STACK}
        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
        MOV     ESI,EDX

@@loop:
        MOV     EDX,[EBX]                       { fetch str                     }
        TEST    EDX,EDX                         { if nil, nothing to do         }
        JE      @@doneEntry
        MOV     dword ptr [EBX],0               { clear str                     }
        MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                  }
        DEC     ECX                             { if < 0: literal str           }
        JL      @@doneEntry
   LOCK DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount       }
        JNE     @@doneEntry
        LEA     EAX,[EDX-skew].StrRec.codePage  { if refCnt now zero, deallocate}
        CALL    _FreeMem
@@doneEntry:
        ADD     EBX,4
        DEC     ESI
        JNE     @@loop

        POP     ESI
        POP     EBX
        {$IFDEF ALIGN_STACK}
        ADD     ESP,4
        {$ENDIF ALIGN_STACK}
end;
{$ENDIF !PUREPASCAL}




procedure _WStrArrayClr(var StrArray; Count: Integer);
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
  _UStrArrayClr(StrArray, Count);
end;
{$ELSE}
asm
        JMP     _UStrArrayClr
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
var
  S: PPointer;
  P: Pointer;
begin
  S := PPointer(@StrArray);
  while Count > 0 do
  begin
    P := S^;
    if P <> nil then
    begin
      S^ := nil;
      SysFreeString(WideString(P));
    end;
    Inc(S);
    Dec(Count);
  end;
end;
{$ELSE}
asm
        { ->    EAX pointer to str      }
        {       EDX cnt                 }

        PUSH    EBX
        PUSH    ESI
        MOV     EBX,EAX
        MOV     ESI,EDX
@@1:    MOV     EAX,[EBX]
        TEST    EAX,EAX
        JE      @@2
        MOV     DWORD PTR [EBX],0
        PUSH    EAX
        CALL    SysFreeString
@@2:    ADD     EBX,4
        DEC     ESI
        JNE     @@1
        POP     ESI
        POP     EBX
end;
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}





function _UStrAddRef(Str: Pointer): Pointer;
{$IF not defined(CPU386)}
var
  P: PStrRec;
begin
  Result := Str;
  if Str <> nil then
  begin
    P := Pointer(NativeInt(Str) - SizeOf(StrRec));
    if P.refcnt >= 0 then
      InterlockedIncrement(P.refcnt);
  end;
end;
{$ELSE}
asm
        { ->    EAX     str     }
        TEST    EAX,EAX
        JE      @@exit
        MOV     EDX,[EAX-skew].StrRec.refCnt
        INC     EDX
        JLE     @@exit
   LOCK INC     [EAX-skew].StrRec.refCnt
@@exit:
end;
{$IFEND}




function _LStrAddRef(Str: Pointer): Pointer;
{$IF not defined(CPU386)}
var
  P: PStrRec;
begin
  Result := Str;
  if Str <> nil then
  begin
    P := Pointer(NativeInt(Str) - SizeOf(StrRec));
    if P.refcnt >= 0 then
      InterlockedIncrement(P.refcnt);
  end;
end;
{$ELSE}
asm
        { ->    EAX     str     }
        TEST    EAX,EAX
        JE      @@exit
        MOV     EDX,[EAX-skew].StrRec.refCnt
        INC     EDX
        JLE     @@exit
   LOCK INC     [EAX-skew].StrRec.refCnt
@@exit:
end;
{$IFEND}


// Note: Windows version of WideString is single reference.
//       Only _WStrAddRef for Windows of *StrAddRef versions has
//       'var' parameter.


{$IFDEF POSIX}
function _WStrAddRef(Str: Pointer): Pointer;
{$IFDEF PUREPASCAL}
begin
  Result := _UStrAddRef(Str);
end;
{$ELSE}
asm
        JMP     _UStrAddRef
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}

{$IFDEF MSWINDOWS}
function _WStrAddRef(var Str: WideString): Pointer;
{$IFDEF PUREPASCAL}
var
  P: Pointer;
  Len: LongInt;
begin
  Result := Pointer(Str);
  if Pointer(Str) <> nil then
  begin
    Len := PLongInt(NativeInt(Pointer(Str)) - Sizeof(LongInt))^ div 2;
    Result := Pointer(SysAllocStringLen(PWideChar(Pointer(Str)), Len));
    if Result = nil then
      WStrError;
    Pointer(Str) := Result;
  end;
end;
{$ELSE}
asm
        { ->    EAX     pointer to WideString   }
        { <-    EAX     str                     }
        MOV     EDX,[EAX]
        TEST    EDX,EDX
        JE      @@1
        PUSH    EAX
        MOV     ECX,[EDX-4]
        SHR     ECX,1
        PUSH    ECX
        PUSH    EDX
        CALL    SysAllocStringLen
        POP     EDX
        TEST    EAX,EAX
        JE      WStrError
        MOV     [EDX],EAX
@@1:
end;
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}


{ ----------------------------------------------------- }
{       internal string conversion                      }
{ ----------------------------------------------------- }



function CharFromWChar(CharDest: PAnsiChar; DestBytes: Integer; const WCharSource: PWideChar; SrcChars: Integer; CodePage: Integer): Integer;
begin
  if CodePage = 0 then
    CodePage := DefaultSystemCodePage;
  Result := LocaleCharsFromUnicode(CodePage, 0, WCharSource, SrcChars, CharDest,
    DestBytes, nil, nil);
end;

function CharFromWChar(CharDest: PAnsiChar; DestBytes: Integer; const WCharSource: PWideChar; SrcChars: Integer): Integer;
begin
  Result := CharFromWChar(CharDest, DestBytes, WCharSource, SrcChars, DefaultSystemCodePage);
end;


function WCharFromChar(WCharDest: PWideChar; DestChars: Integer; const CharSource: PAnsiChar; SrcBytes: Integer; CodePage: Integer): Integer;
begin
  Result := UnicodeFromLocaleChars(CodePage, 0, CharSource, SrcBytes, WCharDest,
    DestChars);
end;


{ ----------------------------------------------------- }
{       basic string constructors                       }
{ ----------------------------------------------------- }


procedure _UStrFromPWCharLen(var Dest: UnicodeString; Source: PWideChar; CharLength: Integer);
{$IFDEF PUREPASCAL}
var
  Temp: Pointer;
begin
  Temp := Pointer(Dest);
  if CharLength > 0 then
  begin
    Pointer(Dest) := _NewUnicodeString(CharLength);
    if Source <> nil then
      Move(Source^, Pointer(Dest)^, CharLength * SizeOf(WideChar));
  end
  else
    Pointer(Dest) := nil;
  _UStrClr(Temp);
end;
{$ELSE}
asm
        { ->    EAX     pointer to dest         }
        {       EDX     source                  }
        {       ECX     length in characters    }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX // EBX := addr of Dest (result) in EBX
        MOV     ESI,EDX // ESI := source
        MOV     EDI,ECX // EDI := length

        { allocate new string }

        MOV     EAX,EDI // EAX := length

        CALL    _NewUnicodeString // EAX := new string (result)
        MOV     ECX,EDI // ECX := length
        MOV     EDI,EAX // EDI := result

        TEST    ESI,ESI // nil source?
        JE      @@noMove

        MOV     EDX,EAX // EDX := result (dest for Move)
        MOV     EAX,ESI // EAX := source (source for Move)
        SHL     ECX,1   // ECX := ECX * 2 (turn length into characters)
        CALL    Move

        { assign the result to dest }

@@noMove:
        MOV     EAX,EBX
        CALL    _LStrClr
        MOV     [EBX],EDI

        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF !PUREPASCAL}


procedure _WStrFromPWCharLen(var Dest: WideString; Source: PWideChar; CharLength: Integer);
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
  _UStrFromPWCharLen(UnicodeString(Pointer(Dest)), Source, CharLength);
end;
{$ELSE}
asm
        JMP     _UStrFromPWCharLen
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}

{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
var
  Temp: Pointer;
begin
  if CharLength <= 0 then
    _WStrClr(Dest)
  else
  begin
    Temp := SysAllocStringLen(Source, CharLength);
    if Temp = nil then
      WStrError;
  //  Temp := InterlockedExchangePointer(Pointer(Dest), Temp);
  //  if Temp <> nil then
  //    SysFreeString(WideString(Temp));
    if Pointer(Dest) <> nil then
      SysFreeString(Dest);
    Pointer(Dest) := Temp;
  end;
end;
{$ELSE}
asm
        { ->    EAX     Pointer to WideString (dest)      }
        {       EDX     Pointer to characters (source)    }
        {       ECX     number of characters  (not bytes) }
        TEST    ECX,ECX
        JE      _WStrClr

        PUSH    EAX

        PUSH    ECX
        PUSH    EDX
        CALL    SysAllocStringLen
        TEST    EAX,EAX
        POP     EDX
        JE      WStrError

        {$IFDEF ALIGN_STACK}
        SUB     ESP,8
        {$ENDIF ALIGN_STACK}
        PUSH    [EDX].PWideChar
        MOV     [EDX],EAX

        CALL    SysFreeString
        {$IFDEF ALIGN_STACK}
        ADD     ESP,8
        {$ENDIF ALIGN_STACK}
end;
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}



procedure _LStrFromPCharLen(var Dest: AnsiString; Source: PAnsiChar; Length: Integer; CodePage: Word);
{$IF not defined(CPU386)}
var
  P: PAnsiChar;
begin
  P := _NewAnsiString(Length, CodePage);
  if Source <> nil then
    Move(Source^, P^, Length);
  _LStrClr(Dest);
  Pointer(Dest) := P;
end;
{$ELSE}
asm //StackAlignSafe
        { ->    EAX     pointer to dest }
        {       EDX     source          }
        {       ECX     length          }
        {       [ESP+0] caller EBP      }
        {       [ESP+4] return address  }
        {       [ESP+8] CodePage        }
{$IFDEF ALIGN_STACK}
        // EBP is already pushed on the stack
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        { allocate new string }

        MOV     EAX,EDI
        MOVZX   EDX,CodePage

        CALL    _NewAnsiString
        MOV     ECX,EDI
        MOV     EDI,EAX

        TEST    ESI,ESI
        JE      @@noMove

        MOV     EDX,EAX
        MOV     EAX,ESI
        CALL    Move

        { assign the result to dest }

@@noMove:
        MOV     EAX,EBX
        CALL    _LStrClr
        MOV     [EBX],EDI

        POP     EDI
        POP     ESI
        POP     EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
end;
{$IFEND}

procedure InternalUStrFromPCharLen(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer; CodePage: Integer);
var
  DestLen: Integer;
  Buffer: array[0..2047] of WideChar;
begin
  if Length <= 0 then
  begin
    _UStrClr(Dest);
    Exit;
  end;
  if Length+1 < High(Buffer) then
  begin
    DestLen := WCharFromChar(Buffer, High(Buffer), Source, Length, CodePage);
    if DestLen > 0 then
    begin
      _UStrFromPWCharLen(Dest, @Buffer, DestLen);
      Exit;
    end;
  end;

  DestLen := (Length + 1);
  _UStrSetLength(Dest, DestLen);  // overallocate, trim later
  DestLen := WCharFromChar(Pointer(Dest), DestLen, Source, Length, CodePage);
  if DestLen < 0 then
    DestLen := 0;
  _UStrSetLength(Dest, DestLen);

end;

procedure _UStrFromPCharLen(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer);
begin
  InternalUStrFromPCharLen(Dest, Source, Length, DefaultSystemCodePage);
end;

procedure InternalWStrFromPCharLen(var Dest: WideString; Source: PAnsiChar; Length: Integer; CodePage: Integer);
var
  DestLen: Integer;
  Buffer: array[0..2047] of WideChar;
begin
  if Length <= 0 then
  begin
    _WStrClr(Dest);
    Exit;
  end;
  if Length+1 < High(Buffer) then
  begin
    DestLen := WCharFromChar(Buffer, High(Buffer), Source, Length, CodePage);
    if DestLen > 0 then
    begin
      _WStrFromPWCharLen(Dest, @Buffer, DestLen);
      Exit;
    end;
  end;

  DestLen := (Length + 1);
  _WStrSetLength(Dest, DestLen);  // overallocate, trim later
  DestLen := WCharFromChar(PWideChar(Pointer(Dest)), DestLen, Source, Length, CodePage);
  if DestLen < 0 then DestLen := 0;
  _WStrSetLength(Dest, DestLen);

end;

procedure _WStrFromPCharLen(var Dest: WideString; Source: PAnsiChar; Length: Integer);
begin
  InternalWStrFromPCharLen(Dest, Source, Length, DefaultSystemCodePage);
end;

procedure _LStrFromPWCharLen(var Dest: AnsiString; Source: PWideChar; Length: Integer; CodePage: Word);
var
  DestLen: Integer;
begin
  if Length <= 0 then
  begin
    _LStrClr(Dest);
    Exit;
  end;

  if CodePage = 0 then
    CodePage := DefaultSystemCodePage;

  DestLen := CharFromWChar(nil, 0, Source, Length, CodePage);
  SetLength(Dest, DestLen);

  if DestLen > 0 then
  begin
    CharFromWChar(Pointer(Dest), DestLen, Source, Length, CodePage);
    PStrRec(NativeInt(Pointer(Dest)) - SizeOf(StrRec)).codePage := CodePage;
  end
  else
    _LStrClr(Dest);
end;


{ ----------------------------------------------------- }
{       Compiler helper for string assignment           }
{ ----------------------------------------------------- }



procedure _UStrAsg(var Dest: UnicodeString; const Source: UnicodeString); // globals (need copy)
{$IFDEF PUREPASCAL}
var
  S, D: Pointer;
  P: PStrRec;
  Len: LongInt;
begin
  S := Pointer(Source);
  if S <> nil then
  begin
    if __StringRefCnt(Source) < 0 then   // make copy of string literal
    begin
      Len := __StringLength(Source);
      S := _NewUnicodeString(Len);
      Move(Pointer(Source)^, S^, Len * SizeOf(WideChar));
    end else
    begin
      P := PStrRec(NativeInt(S) - SizeOf(StrRec));
      InterlockedIncrement(P.refCnt);
    end;
  end;
  D := Pointer(Dest);
  Pointer(Dest) := S;
  _UStrClr(D);
end;
{$ELSE}
asm
        { ->    EAX pointer to dest   str       }
        { ->    EDX pointer to source str       }

        TEST    EDX,EDX                         { have a source? }
        JE      @@2                             { no -> jump     }

        MOV     ECX,[EDX-skew].StrRec.refCnt
        INC     ECX
        JG      @@1                             { literal string -> jump not taken }

        {$IFDEF ALIGN_STACK}
        SUB     ESP,4
        {$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        MOV     EAX,[EDX-skew].StrRec.length
        CALL    _NewUnicodeString
        MOV     EDX,EAX
        POP     EAX
        PUSH    EDX
        MOV     ECX,[EAX-skew].StrRec.length
        SHL     ECX,1                           { length to bytes for move }
        CALL    Move
        POP     EDX
        POP     EAX
        {$IFDEF ALIGN_STACK}
        ADD     ESP,4
        {$ENDIF ALIGN_STACK}
        JMP     @@2

@@1:
   LOCK INC     [EDX-skew].StrRec.refCnt

@@2:    XCHG    EDX,[EAX]
        TEST    EDX,EDX
        JE      @@3
        MOV     ECX,[EDX-skew].StrRec.refCnt
        DEC     ECX
        JL      @@3
   LOCK DEC     [EDX-skew].StrRec.refCnt
        JNE     @@3
        LEA     EAX,[EDX-skew].StrRec.codePage
        {$IFDEF ALIGN_STACK}
        SUB     ESP,12
        {$ENDIF ALIGN_STACK}
        CALL    _FreeMem
        {$IFDEF ALIGN_STACK}
        ADD     ESP,12
        {$ENDIF ALIGN_STACK}
@@3:
end;
{$ENDIF !PUREPASCAL}




procedure _UStrLAsg(var Dest: UnicodeString; const Source: UnicodeString); // locals
{$IF not defined(CPU386)}
var
  P: Pointer;
begin
  if Pointer(Source) <> nil then
    _UStrAddRef(Pointer(Source));
  P := Pointer(Dest);
  Pointer(Dest) := Pointer(Source);
  _UStrClr(P);
end;
{$ELSE}
asm
        { ->    EAX     pointer to dest }
        {       EDX     source          }

        TEST    EDX,EDX
        JE      @@sourceDone

        { bump up the ref count of the source }

        MOV     ECX,[EDX-skew].StrRec.refCnt
        INC     ECX
        JLE     @@sourceDone                    { literal assignment -> jump taken }
   LOCK INC     [EDX-skew].StrRec.refCnt
@@sourceDone:

        { we need to release whatever the dest is pointing to   }

        XCHG    EDX,[EAX]                       { fetch str                    }
        TEST    EDX,EDX                         { if nil, nothing to do        }
        JE      @@done
        MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                 }
        DEC     ECX                             { if < 0: literal str          }
        JL      @@done
   LOCK DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount      }
        JNE     @@done
        LEA     EAX,[EDX-skew].StrRec.codePage  { if refCnt now zero, deallocate}
        {$IFDEF ALIGN_STACK}
        SUB     ESP,12
        {$ENDIF ALIGN_STACK}
        CALL    _FreeMem
        {$IFDEF ALIGN_STACK}
        ADD     ESP,12
        {$ENDIF ALIGN_STACK}
@@done:
end;
{$IFEND}




procedure _WStrAsg(var Dest: WideString; const Source: WideString);
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
  _UStrAsg(UnicodeString(Pointer(Dest)), UnicodeString(Pointer(Source)));
end;
{$ELSE}
asm
        { ->    EAX     Pointer to WideString }
        {       EDX     Pointer to data       }

        JMP     _UStrAsg
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}

{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
var
  Len: Integer;
begin
  if Pointer(Dest) <> Pointer(Source) then
  begin
    if Pointer(Source) = nil then
      _WStrClr(Dest)
    else
    begin
      Len := __StringLength(Source);
      if Len = 0 then
        _WStrClr(Dest)
      else
      begin
        if not SysReAllocStringLen(Dest, PWideChar(Pointer(Source)), Len) then
          WStrError;
      end;
    end;
  end;
end;
{$ELSE}
asm
        { ->    EAX     Pointer to WideString }
        {       EDX     Pointer to data       }
        CMP     [EAX],EDX
        JE      @@1
        TEST    EDX,EDX
        JE      _WStrClr
        MOV     ECX,[EDX-4]
        SHR     ECX,1
        JE      _WStrClr
        PUSH    ECX
        PUSH    EDX
        PUSH    EAX
        CALL    SysReAllocStringLen
        TEST    EAX,EAX
        JE      WStrError
@@1:
end;
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}



procedure _WStrLAsg(var Dest: WideString; const Source: WideString);
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
  _UStrLAsg(UnicodeString(Pointer(Dest)), UnicodeString(Pointer(Source)));
end;
{$ELSE}
asm
        JMP     _UStrLAsg
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}

{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
begin
  _WStrAsg(Dest, Source);
end;
{$ELSE}
asm
        JMP   _WStrAsg
end;
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}


{ 99.03.11
  This function is used when assigning to global variables.

  Literals are copied to prevent a situation where a dynamically
  allocated DLL or package assigns a literal to a variable and then
  is unloaded -- thereby causing the string memory (in the code
  segment of the DLL) to be removed -- and therefore leaving the
  global variable pointing to invalid memory.
}



{$IFDEF PUREPASCAL}
procedure _LStrAsg(var Dest: AnsiString; const Source: AnsiString);
var
  S, D: Pointer;
  P: PStrRec;
  Len: LongInt;
begin
  S := Pointer(Source);
  if S <> nil then
  begin
    if __StringRefCnt(S) < 0 then   // make copy of string literal
    begin
      Len := __StringLength(S);
      S := _NewAnsiString(Len, __StringCodePage(S));
      Move(PAnsiChar(Source)^, S^, Len);
    end else
    begin
      P := PStrRec(NativeInt(S) - SizeOf(StrRec));
      InterlockedIncrement(P.refCnt);
    end;
  end;
  D := Pointer(Dest);
  Pointer(Dest) := S;
  _LStrClr(D);
end;
{$ELSE}
procedure _LStrAsg(var dest; const source);
asm
        { ->    EAX pointer to dest   str       }
        { ->    EDX pointer to source str       }

        TEST    EDX,EDX                         { have a source? }
        JE      @@2                             { no -> jump     }

        MOV     ECX,[EDX-skew].StrRec.refCnt
        INC     ECX
        JG      @@1                             { literal string -> jump not taken }

{$IFDEF ALIGN_STACK}
        SUB     ESP,4
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        MOV     EAX,[EDX-skew].StrRec.length
        MOVZX   EDX,[EDX-skew].StrRec.codePage
        CALL    _NewAnsiString
        MOV     EDX,EAX
        POP     EAX
        PUSH    EDX
        MOV     ECX,[EAX-skew].StrRec.length
        CALL    Move
        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP,4
{$ENDIF ALIGN_STACK}
        JMP     @@2

@@1:
   LOCK INC     [EDX-skew].StrRec.refCnt

@@2:    XCHG    EDX,[EAX]
        TEST    EDX,EDX
        JE      @@3
        MOV     ECX,[EDX-skew].StrRec.refCnt
        DEC     ECX
        JL      @@3
   LOCK DEC     [EDX-skew].StrRec.refCnt
        JNE     @@3
        LEA     EAX,[EDX-skew].StrRec.codePage // Beginning of StrRec
{$IFDEF ALIGN_STACK}
        SUB     ESP,12
{$ENDIF ALIGN_STACK}
        CALL    _FreeMem
{$IFDEF ALIGN_STACK}
        ADD     ESP,12
{$ENDIF ALIGN_STACK}
@@3:
end;
{$ENDIF !PUREPASCAL}





{$IFDEF PUREPASCAL}
procedure _LStrLAsg(var Dest: AnsiString; const Source: AnsiString);
var
  P: Pointer;
begin
  P := Pointer(Source);
  if P <> nil then
    _LStrAddRef(P);
  P := Pointer(Dest);
  Pointer(Dest) := Pointer(Source);
  _LStrClr(P);
end;
{$ELSE}
procedure _LStrLAsg(var dest; const source);
asm
        { ->    EAX     pointer to dest }
        {       EDX     source          }

        TEST    EDX,EDX
        JE      @@sourceDone

        { bump up the ref count of the source }

        MOV     ECX,[EDX-skew].StrRec.refCnt
        INC     ECX
        JLE     @@sourceDone                    { literal assignment -> jump taken }
   LOCK INC     [EDX-skew].StrRec.refCnt
@@sourceDone:

        { we need to release whatever the dest is pointing to   }

        XCHG    EDX,[EAX]                       { fetch str                    }
        TEST    EDX,EDX                         { if nil, nothing to do        }
        JE      @@done
        MOV     ECX,[EDX-skew].StrRec.refCnt    { fetch refCnt                 }
        DEC     ECX                             { if < 0: literal str          }
        JL      @@done
   LOCK DEC     [EDX-skew].StrRec.refCnt        { threadsafe dec refCount      }
        JNE     @@done
        LEA     EAX,[EDX-skew].StrRec.codePage  { if refCnt now zero, deallocate}
{$IFDEF ALIGN_STACK}
        SUB     ESP,12
{$ENDIF ALIGN_STACK}
        CALL    _FreeMem
{$IFDEF ALIGN_STACK}
        ADD     ESP,12
{$ENDIF ALIGN_STACK}
@@done:
end;
{$ENDIF !PUREPASCAL}


{ ----------------------------------------------------- }
{       string info utilities                           }
{ ----------------------------------------------------- }

function StringElementSize(const S: UnicodeString): Word; overload;
begin
  if S <> '' then
    Result := PWord(NativeInt(S) - 10)^                          // StrRec.elemSize
  else
    Result := SizeOf(WideChar);
end;

function StringElementSize(const S: RawByteString): Word; overload;
begin
  if S <> '' then
    Result := PWord(NativeInt(S) - 10)^                          // StrRec.elemSize
  else
    Result := SizeOf(AnsiChar);
end;

{$IFNDEF MSWINDOWS}
function StringElementSize(const S: WideString): Word; overload;
begin
  if S <> '' then
    Result := PWord(NativeInt(S) - 10)^                          // StrRec.elemSize
  else
    Result := SizeOf(WideChar);
end;
{$ENDIF !MSWINDOWS}

function StringCodePage(const S: UnicodeString): Word; overload;
begin
  if S <> '' then
    Result := PWord(NativeInt(S) - 12)^                          // StrRec.codePage
  else
    Result := Word(DefaultUnicodeCodePage);
end;

function StringCodePage(const S: RawByteString): Word; overload;
begin
  if S <> '' then
    Result := PWord(NativeInt(S) - 12)^                          // StrRec.codePage
  else
    Result := Word(DefaultSystemCodePage);
end;

{$IFNDEF MSWINDOWS}
function StringCodePage(const S: WideString): Word; overload;
begin
  if S <> '' then
    Result := PWord(NativeInt(S) - 12)^                          // StrRec.codePage
  else
    Result := Word(DefaultUnicodeCodePage);
end;
{$ENDIF !MSWINDOWS}


function StringRefCount(const S: UnicodeString): Longint;
{$IF defined(CPUX64)}
begin
  Result := 0;
  if Pointer(S) <> nil then                // PStrRec should be used here, but
    Result := PLongInt(NativeInt(S) - 8)^; // a private symbol can't be inlined
end;
{$ELSE}
begin
  Result := Longint(S);
  if Result <> 0 then                // PStrRec should be used here, but
    Result := PLongint(Result - 8)^; // a private symbol can't be inlined
end;
{$IFEND}


function StringRefCount(const S: RawByteString): Longint;
{$IF defined(CPUX64)}
begin
  Result := 0;
  if Pointer(S) <> nil then                // PStrRec should be used here, but
    Result := PLongInt(NativeInt(S) - 8)^; // a private symbol can't be inlined
end;
{$ELSE}
begin
  Result := Longint(S);
  if Result <> 0 then                // PStrRec should be used here, but
    Result := PLongint(Result - 8)^; // a private symbol can't be inlined
end;
{$IFEND}

{$IFNDEF MSWINDOWS}
function StringRefCount(const S: WideString): Integer;
{$IF defined(CPUX64)}
begin
  Result := 0;
  if Pointer(S) <> nil then                // PStrRec should be used here, but
    Result := PLongInt(NativeInt(S) - 8)^; // a private symbol can't be inlined
end;
{$ELSE}
begin
  Result := LongInt(S);
  if Result <> 0 then                // PStrRec should be used here, but
    Result := PLongInt(Result - 8)^; // a private symbol can't be inlined
end;
{$IFEND}
{$ENDIF !MSWINDOWS}


{ ----------------------------------------------------- }
{       Compiler helper for string length               }
{ ----------------------------------------------------- }


function _UStrLen(const S: UnicodeString): Integer;
{$IF defined(CPUX64)}
begin
  Result := 0;
  if Pointer(S) <> nil then                // PStrRec should be used here, but
    Result := PLongInt(NativeInt(S) - 4)^; // a private symbol can't be inlined
end;
{$ELSE}
begin
  Result := Longint(S);
  if Result <> 0 then                // PStrRec should be used here, but
    Result := PLongint(Result - 4)^; // a private symbol can't be inlined
end;
{$IFEND}


function _WStrLen(const S: WideString): Longint; inline;
{$IF defined(CPUX64)}
begin
  Result := 0;
  if Pointer(S) <> nil then
    {$IFDEF MSWINDOWS}
    Result := PLongInt(NativeInt(S) - 4)^ shr 1;
    {$ELSE}
    Result := PLongInt(NativeInt(S) - 4)^;
    {$ENDIF}
end;
{$ELSE}
begin
  Result := Longint(S);
  if Result <> 0 then
    {$IFDEF MSWINDOWS}
    Result := PLongInt(Result - 4)^ shr 1;
    {$ELSE}
    Result := PLongInt(Result - 4)^;
    {$ENDIF}
end;
{$IFEND}


function _LStrLen(const S: AnsiString): Longint;
{$IF defined(CPUX64)}
begin
  Result := 0;
  if Pointer(S) <> nil then                // PStrRec should be used here, but
    Result := PLongInt(NativeInt(S) - 4)^; // a private symbol can't be inlined
end;
{$ELSE}
begin
  Result := Longint(S);
  if Result <> 0 then                // PStrRec should be used here, but
    Result := PLongint(Result - 4)^; // a private symbol can't be inlined
end;
{$IFEND}


{$IF not defined(CPU386)}
function _PStrLen(const str: ShortString): Integer; inline;
begin
  Result := Byte(str[0]);
end;
{$IFEND}




function _PCharLen(P: PAnsiChar): Longint;
{$IF not defined(CPU386)}
var
  Temp: PAnsiChar;
begin
  Temp := P;
  if Temp <> nil then
    while Temp^ <> #0 do Inc(Temp);
  Result := Temp - P;
end;
{$ELSE}
asm
        TEST    EAX,EAX
        JE      @@5
        PUSH    EAX
        XOR     ECX,ECX
@@0:    CMP     CL,[EAX+0]
        JE      @@4
        CMP     CL,[EAX+1]
        JE      @@3
        CMP     CL,[EAX+2]
        JE      @@2
        CMP     CL,[EAX+3]
        JE      @@1
        ADD     EAX,4
        JMP     @@0
@@1:    INC     EAX
@@2:    INC     EAX
@@3:    INC     EAX
@@4:    POP     ECX
        SUB     EAX,ECX
@@5:
end;
{$IFEND}




function _PWCharLen(P: PWideChar): Longint;
{$IF not defined(CPU386)}
var
  S: PWideChar;
begin
  S := P;
  if P <> nil then
    while P^ <> #0 do
      Inc(P);
  Result := P - S;
end;
{$ELSE}
asm
        TEST    EAX,EAX
        JE      @@5
        PUSH    EAX
        XOR     ECX,ECX
@@0:    CMP     CX,[EAX+0]
        JE      @@4
        CMP     CX,[EAX+2]
        JE      @@3
        CMP     CX,[EAX+4]
        JE      @@2
        CMP     CX,[EAX+6]
        JE      @@1
        ADD     EAX,8
        JMP     @@0
@@1:    ADD     EAX,2
@@2:    ADD     EAX,2
@@3:    ADD     EAX,2
@@4:    POP     ECX
        SUB     EAX,ECX
        SHR     EAX,1
@@5:
end;
{$IFEND}


{ ----------------------------------------------------- }
{       internal UniqueString* support functions        }
{ ----------------------------------------------------- }




function InternalUniqueStringU(var Str: UnicodeString): Pointer;
{$IF not defined(CPU386)}
var
  P: PStrRec;
begin
  Result := Pointer(Str);
  if Result <> nil then
  begin
    Result := Pointer(Str);
    P := Pointer(NativeInt(Str) - SizeOf(StrRec));
    if P.refCnt <> 1 then
    begin
      Result := _NewUnicodeString(P.length);
      Move(PWideChar(Str)^, PWideChar(Result)^, P.length * SizeOf(WideChar));
      _UStrClr(Str);
      Pointer(Str) := Result;
    end;
  end;
end;
{$ELSE}
asm
        { ->    EAX pointer to str              }
        { <-    EAX pointer to unique copy      }
        MOV     EDX,[EAX]       // EDX := str
        TEST    EDX,EDX         // nil?
        JE      @@exit
        MOV     ECX,[EDX-skew].StrRec.refCnt // ECX := str.refCnt
        DEC     ECX             // refCnt = 1?
        JE      @@exit

        PUSH    EBX
        {$IFDEF ALIGN_STACK}
        SUB     ESP, 8
        {$ENDIF ALIGN_STACK}
        MOV     EBX,EAX         // EBX := @str
        MOV     EAX,[EDX-skew].StrRec.length
        CALL    _NewUnicodeString
        MOV     EDX,EAX         // EDX := newStr
        XCHG    EAX,[EBX]       // EAX := str ; @str^ := newStr
        {$IFDEF ALIGN_STACK}
        MOV     [ESP],EAX       // save str
        {$ELSE !ALIGN_STACK}
        PUSH    EAX             // save str
        {$ENDIF !ALIGN_STACK}
        MOV     ECX,[EAX-skew].StrRec.length
        SHL     ECX,1           // ECX := Length(str) * 2
        CALL    Move            // Move(str, newStr, Length(str) * 2)
        {$IFDEF ALIGN_STACK}
        MOV    EAX,[ESP]        // EAX := str
        {$ELSE !ALIGN_STACK}
        POP     EAX             // EAX := str
        {$ENDIF !ALIGN_STACK}
        MOV     ECX,[EAX-skew].StrRec.refCnt // ECX := str.refCnt
        DEC     ECX
        JL      @@skip          // Was already zero?
   LOCK DEC     [EAX-skew].StrRec.refCnt
        JNZ     @@skip
        LEA     EAX,[EAX-skew].StrRec.codePage  { if refCnt now zero, deallocate}
        CALL    _FreeMem
@@skip:
        MOV     EDX,[EBX]       // EDX := @str^ (= newStr)
        {$IFDEF ALIGN_STACK}
        ADD     ESP, 8
        {$ENDIF ALIGN_STACK}
        POP     EBX
@@exit:
        MOV     EAX,EDX         // EAX := newStr
end;
{$IFEND}





function InternalUniqueStringA(var Str: AnsiString): Pointer;
{$IF not defined(CPU386)}
var
  P: PStrRec;
begin
  Result := Pointer(Str);
  if Result <> nil then
  begin
    Result := Pointer(Str);
    P := Pointer(NativeInt(Str) - sizeof(StrRec));
    if P.refCnt <> 1 then
    begin
      Result := _NewAnsiString(P.length, P.codePage);
      Move(PAnsiChar(Str)^, PAnsiChar(Result)^, P.length);
      _LStrClr(Str);
      Pointer(Str) := Result;
    end;
  end;
end;
{$ELSE}
asm
        { ->    EAX pointer to str              }
        { <-    EAX pointer to unique copy      }
        MOV     EDX,[EAX]
        TEST    EDX,EDX
        JE      @@exit
        MOV     ECX,[EDX-skew].StrRec.refCnt
        DEC     ECX
        JE      @@exit

        PUSH    EBX
        {$IFDEF ALIGN_STACK}
        SUB     ESP, 8
        {$ENDIF ALIGN_STACK}
        MOV     EBX,EAX
        MOV     EAX,[EDX-skew].StrRec.length
        MOVZX   EDX,[EDX-skew].StrRec.codePage
        CALL    _NewAnsiString
        MOV     EDX,EAX
        XCHG    EAX,[EBX]       // EAX := str ; @str^ := newStr
        {$IFDEF ALIGN_STACK}
        MOV     [ESP],EAX       // save str
        {$ELSE !ALIGN_STACK}
        PUSH    EAX
        {$ENDIF !ALIGN_STACK}
        MOV     ECX,[EAX-skew].StrRec.length
        CALL    Move
        {$IFDEF ALIGN_STACK}
        MOV     EAX,[ESP]       // EAX := str
        {$ELSE !ALIGN_STACK}
        POP     EAX
        {$ENDIF !ALIGN_STACK}
        MOV     ECX,[EAX-skew].StrRec.refCnt
        DEC     ECX
        JL      @@skip
   LOCK DEC     [EAX-skew].StrRec.refCnt
        JNZ     @@skip
        LEA     EAX,[EAX-skew].StrRec.codePage  { if refCnt now zero, deallocate}
        CALL    _FreeMem
@@skip:
        MOV     EDX,[EBX]
        {$IFDEF ALIGN_STACK}
        ADD     ESP, 8
        {$ENDIF ALIGN_STACK}
        POP     EBX
@@exit:
        MOV     EAX,EDX
end;
{$IFEND}


{ ----------------------------------------------------- }
{       Compiler helper for _UniqueString* functions    }
{ ----------------------------------------------------- }



function _UniqueStringU(var Str: UnicodeString): Pointer;
{$IFDEF PUREPASCAL}
begin
  Result := InternalUniqueStringU(Str);
end;
{$ELSE}
asm
        JMP     InternalUniqueStringU
end;
{$ENDIF !PUREPASCAL}



{$IFNDEF MSWINDOWS}
function _UniqueStringW(var Str: WideString): Pointer;
{$IFDEF PUREPASCAL}
begin
  Result := InternalUniqueStringU(UnicodeString(Pointer(Str)));
end;
{$ELSE}
asm
        JMP     InternalUniqueStringU
end;
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}



function _UniqueStringA(var Str: AnsiString): Pointer;
{$IF not defined(CPU386)}
begin
  Result := InternalUniqueStringA(Str);
end;
{$ELSE}
asm
        JMP     InternalUniqueStringA
end;
{$IFEND}


{ ----------------------------------------------------- }
{       UniqueString* functions                         }
{ ----------------------------------------------------- }



procedure UniqueString(var str: UnicodeString); overload;
{$IF not defined(CPU386)}
begin
  InternalUniqueStringU(str);
end;
{$ELSE}
asm
        JMP     InternalUniqueStringU
end;
{$IFEND}



procedure UniqueString(var str: WideString);
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
  InternalUniqueStringU(UnicodeString(Pointer(str)));
end;
{$ELSE}
asm
        JMP     InternalUniqueStringU
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
begin
  // nothing to do - Windows WideStrings are always single reference
end;
{$ENDIF}



procedure UniqueString(var str: AnsiString);
{$IF not defined(CPU386)}
begin
  InternalUniqueStringA(str);
end;
{$ELSE}
asm
        JMP     InternalUniqueStringA
end;
{$IFEND}


{ ------------------------------------------------------------- }
{       Compiler helper for comparing array of characters       }
{ ------------------------------------------------------------- }

{$IF not defined(CPU386)}

function _PStrCmp(const Left, Right: ShortString): Integer;
var
  L1, L2: Cardinal;
  P1, P2: PByte;
begin
  P1 := PByte(@Left[0]);
  P2 := PByte(@Right[0]);
  L1 := P1^;
  L2 := P2^;
  Inc(P1);
  Inc(P2);
  if L1 > L2 then L1 := L2;
  Result := 0;
  while L1 >= SizeOf(LongWord) do begin
    if PLongWord(P1)^ <> PLongWord(P2)^ then break;
    if L1 < SizeOf(LongWord) * 2 then
    begin
      Inc(P1, SizeOf(LongWord));
      Inc(P2, SizeOf(LongWord));
      Dec(L1, SizeOf(LongWord));
      break;
    end;
    if PLongWord(NativeInt(P1) + SizeOf(LongWord))^ <>
       PLongWord(NativeInt(P2) + SizeOf(LongWord))^ then break;
    Inc(P1, SizeOf(LongWord) * 2);
    Inc(P2, SizeOf(LongWord) * 2);
    Dec(L1, SizeOf(LongWord) * 2);
  end;
  if L1 = 0 then Exit;
  Result := PByte(P1)^ - PByte(P2)^;
  if Result <> 0 then Exit;
  if L1 = 1 then Exit;
  Result := PByte(NativeInt(P1) + 1)^ - PByte(NativeInt(P2) + 1)^;
  if Result <> 0 then Exit;
  if L1 = 2 then Exit;
  Result := PByte(NativeInt(P1) + 2)^ - PByte(NativeInt(P2) + 2)^;
  if Result <> 0 then Exit;
  if L1 = 3 then Exit;
  Result := PByte(NativeInt(P1) + 3)^ - PByte(NativeInt(P2) + 3)^;
end;
{$ELSE}


procedure       _PStrCmp;
asm
        {     ->EAX = Pointer to left string    }
        {       EDX = Pointer to right string   }
        {     <-ZF,CF = Result                  }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX

        XOR     EAX,EAX
        XOR     EDX,EDX
        MOV     AL,[ESI]
        MOV     DL,[EDI]
        INC     ESI
        INC     EDI

        SUB     EAX,EDX { eax = len1 - len2 }
        JA      @@skip1
        ADD     EDX,EAX { edx = len2 + (len1 - len2) = len1     }

@@skip1:
        PUSH    EDX
        SHR     EDX,2
        JE      @@cmpRest
@@longLoop:
        MOV     ECX,[ESI]
        MOV     EBX,[EDI]
        CMP     ECX,EBX
        JNE     @@misMatch
        DEC     EDX
        JE      @@cmpRestP4
        MOV     ECX,[ESI+4]
        MOV     EBX,[EDI+4]
        CMP     ECX,EBX
        JNE     @@misMatch
        ADD     ESI,8
        ADD     EDI,8
        DEC     EDX
        JNE     @@longLoop
        JMP     @@cmpRest
@@cmpRestP4:
        ADD     ESI,4
        ADD     EDI,4
@@cmpRest:
        POP     EDX
        AND     EDX,3
        JE      @@equal

        MOV     CL,[ESI]
        CMP     CL,[EDI]
        JNE     @@exit
        DEC     EDX
        JE      @@equal
        MOV     CL,[ESI+1]
        CMP     CL,[EDI+1]
        JNE     @@exit
        DEC     EDX
        JE      @@equal
        MOV     CL,[ESI+2]
        CMP     CL,[EDI+2]
        JNE     @@exit

@@equal:
        ADD     EAX,EAX
        JMP     @@exit

@@misMatch:
        POP     EDX
        CMP     CL,BL
        JNE     @@exit
        CMP     CH,BH
        JNE     @@exit
        SHR     ECX,16
        SHR     EBX,16
        CMP     CL,BL
        JNE     @@exit
        CMP     CH,BH

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$IFEND}

{$IF not defined(CPU386)}

function _AStrCmp(const Left, Right: PAnsiChar; Len: NativeInt): Integer;
var
  P1, P2: PByte;
begin
  P1 := PByte(Left);
  P2 := PByte(Right);
  Result := 0;
  while Len >= SizeOf(LongWord) do begin
    if PLongWord(P1)^ <> PLongWord(P2)^ then break;
    if Len < SizeOf(LongWord) * 2 then
    begin
      Inc(P1, SizeOf(LongWord));
      Inc(P2, SizeOf(LongWord));
      Dec(Len, SizeOf(LongWord));
      break;
    end;
    if PLongWord(NativeInt(P1) + SizeOf(LongWord))^ <>
       PLongWord(NativeInt(P2) + SizeOf(LongWord))^ then break;
    Inc(P1, SizeOf(LongWord) * 2);
    Inc(P2, SizeOf(LongWord) * 2);
    Dec(Len, SizeOf(LongWord) * 2);
  end;
  if Len = 0 then Exit;
  Result := PByte(P1)^ - PByte(P2)^;
  if Result <> 0 then Exit;
  if Len = 1 then Exit;
  Result := PByte(NativeInt(P1) + 1)^ - PByte(NativeInt(P2) + 1)^;
  if Result <> 0 then Exit;
  if Len = 2 then Exit;
  Result := PByte(NativeInt(P1) + 2)^ - PByte(NativeInt(P2) + 2)^;
  if Result <> 0 then Exit;
  if Len = 3 then Exit;
  Result := PByte(NativeInt(P1) + 3)^ - PByte(NativeInt(P2) + 3)^;
end;
{$ELSE}


procedure       _AStrCmp;
asm
        {     ->EAX = Pointer to left string            }
        {       EDX = Pointer to right string           }
        {       ECX = Number of chars to compare        }
        {     <-ZF,CF = Result                          }

        PUSH    EBX
        PUSH    ESI
        PUSH    ECX
        MOV     ESI,ECX
        SHR     ESI,2
        JE      @@cmpRest

@@longLoop:
        MOV     ECX,[EAX]
        MOV     EBX,[EDX]
        CMP     ECX,EBX
        JNE     @@misMatch
        DEC     ESI
        JE      @@cmpRestP4
        MOV     ECX,[EAX+4]
        MOV     EBX,[EDX+4]
        CMP     ECX,EBX
        JNE     @@misMatch
        ADD     EAX,8
        ADD     EDX,8
        DEC     ESI
        JNE     @@longLoop
        JMP     @@cmpRest
@@cmpRestp4:
        ADD     EAX,4
        ADD     EDX,4
@@cmpRest:
        POP     ESI
        AND     ESI,3
        JE      @@exit

        MOV     CL,[EAX]
        CMP     CL,[EDX]
        JNE     @@exit
        DEC     ESI
        JE      @@equal
        MOV     CL,[EAX+1]
        CMP     CL,[EDX+1]
        JNE     @@exit
        DEC     ESI
        JE      @@equal
        MOV     CL,[EAX+2]
        CMP     CL,[EDX+2]
        JNE     @@exit

@@equal:
        XOR     EAX,EAX
        JMP     @@exit

@@misMatch:
        POP     ESI
        CMP     CL,BL
        JNE     @@exit
        CMP     CH,BH
        JNE     @@exit
        SHR     ECX,16
        SHR     EBX,16
        CMP     CL,BL
        JNE     @@exit
        CMP     CH,BH

@@exit:
        POP     ESI
        POP     EBX
end;
{$IFEND}

{$IF not defined(CPU386)}

function _WStrLCmp(const Left, Right: PWideChar; Len: NativeInt): Integer;
var
  P1, P2: PWord;
begin
  P1 := PWord(Left);
  P2 := PWord(Right);
  Result := 0;
  while Len >= SizeOf(LongWord) div 2 do begin
    if PLongWord(P1)^ <> PLongWord(P2)^ then break;
    if Len < SizeOf(LongWord) then
    begin
      Inc(P1, SizeOf(LongWord));
      Inc(P2, SizeOf(LongWord));
      Dec(Len, SizeOf(LongWord) div 2);
      break;
    end;
    if PLongWord(NativeInt(P1) + SizeOf(LongWord))^ <>
       PLongWord(NativeInt(P2) + SizeOf(LongWord))^ then break;
    Inc(P1, SizeOf(LongWord) * 2);
    Inc(P2, SizeOf(LongWord) * 2);
    Dec(Len, SizeOf(LongWord));
  end;
  if Len = 0 then Exit;
  Result := PWord(P1)^ - PWord(P2)^;
  if Result <> 0 then Exit;
  if Len = 1 then Exit;
  Result := PWord(NativeInt(P1) + 2)^ - PWord(NativeInt(P2) + 2)^;
end;
{$ELSE}


procedure       _WStrLCmp;
asm
        {     ->EAX = Pointer to left wide string       }
        {       EDX = Pointer to right wide string      }
        {       ECX = Number of chars to compare        }
        {     <-ZF,CF = Result                          }

        PUSH    EBX
        PUSH    ESI
        PUSH    ECX
        MOV     ESI,ECX
        SHR     ESI,1
        JE      @@cmpRest

@@longLoop:
        MOV     ECX,[EAX]
        MOV     EBX,[EDX]
        CMP     ECX,EBX
        JNE     @@misMatch
        DEC     ESI
        JE      @@cmpRestP4
        MOV     ECX,[EAX+4]
        MOV     EBX,[EDX+4]
        CMP     ECX,EBX
        JNE     @@misMatch
        ADD     EAX,8
        ADD     EDX,8
        DEC     ESI
        JNE     @@longLoop
        JMP     @@cmpRest
@@cmpRestp4:
        ADD     EAX,4
        ADD     EDX,4
@@cmpRest:
        POP     ESI
        AND     ESI,1
        JE      @@exit

        MOV     CX,[EAX]
        CMP     CX,[EDX]
        JNE     @@exit

@@equal:
        XOR     EAX,EAX
        JMP     @@exit

@@misMatch:
        POP     ESI
        CMP     CX,BX
        JNE     @@exit
        SHR     ECX,16
        SHR     EBX,16
        CMP     CX,BX

@@exit:
        POP     ESI
        POP     EBX
end;
{$IFEND CPU386}


{ ----------------------------------------------------- }
{       Compiler helper for ShortString support         }
{ ----------------------------------------------------- }

procedure       _PStrCpy(Dest: PShortString; Source: PShortString);
begin
  Move(Source^, Dest^, Byte(Source^[0])+1);
end;

procedure       _PStrNCpy(Dest: PShortString; Source: PShortString; MaxLen: Byte);
begin
  if MaxLen > Byte(Source^[0]) then
    MaxLen := Byte(Source^[0]);
  Byte(Dest^[0]) := MaxLen;
  Move(Source^[1], Dest^[1], MaxLen);
end;

{$IF not defined(CPU386)}

procedure _PStrCat(Dest: PShortString; const Src: ShortString);
var
  DestLen, SrcLen, I: Integer;
begin
  DestLen := _PStrLen(Dest^);
  SrcLen := _PStrLen(Src);
  if DestLen + SrcLen > 255 then
    SrcLen := 255 - DestLen;
  Byte(Dest^[0]) := DestLen + SrcLen;
  for I := 1 to SrcLen do
    Dest^[DestLen + I] := Src[I];
end;
{$ELSE}


procedure       _PStrCat;
asm
{     ->EAX = Pointer to destination string     }
{       EDX = Pointer to source string  }

        PUSH    ESI
        PUSH    EDI

{       load dest len into EAX  }

        MOV     EDI,EAX
        XOR     EAX,EAX
        MOV     AL,[EDI]

{       load source address in ESI, source len in ECX   }

        MOV     ESI,EDX
        XOR     ECX,ECX
        MOV     CL,[ESI]
        INC     ESI

{       calculate final length in DL and store it in the destination    }

        MOV     DL,AL
        ADD     DL,CL
        JC      @@trunc

@@cont:
        MOV     [EDI],DL

{       calculate final dest address    }

        INC     EDI
        ADD     EDI,EAX

{       do the copy     }

        REP     MOVSB



        POP     EDI
        POP     ESI
        RET

@@trunc:
        INC     DL      {       DL = #chars to truncate                 }
        SUB     CL,DL   {       CL = source len - #chars to truncate    }
        MOV     DL,255  {       DL = maximum length                     }
        JMP     @@cont
end;
{$IFEND}

{$IF not defined(CPU386)}

procedure _PStrNCat(Dest: PShortString; const Src: ShortString; Size:Integer);
var
  DestLen, SrcLen, I: Integer;
begin
  DestLen := _PStrLen(Dest^);
  SrcLen := _PStrLen(Src);
  if DestLen + SrcLen > Size then
    SrcLen := Size - DestLen;
  Byte(Dest^[0]) := DestLen + SrcLen;
  for I := 1 to SrcLen do
    Dest^[DestLen + I] := Src[I];
end;
{$ELSE}


procedure       _PStrNCat;
asm
{     ->EAX = Pointer to destination string                     }
{       EDX = Pointer to source string                          }
{       CL  = max length of result (allocated size of dest - 1) }

        PUSH    ESI
        PUSH    EDI

{       load dest len into EAX  }

        MOV     EDI,EAX
        XOR     EAX,EAX
        MOV     AL,[EDI]

{       load source address in ESI, source len in EDX   }

        MOV     ESI,EDX
        XOR     EDX,EDX
        MOV     DL,[ESI]
        INC     ESI

{       calculate final length in AL and store it in the destination    }

        ADD     AL,DL
        JC      @@trunc
        CMP     AL,CL
        JA      @@trunc

@@cont:
        MOV     ECX,EDX
        MOV     DL,[EDI]
        MOV     [EDI],AL

{       calculate final dest address    }

        INC     EDI
        ADD     EDI,EDX

{       do the copy     }

        REP     MOVSB

@@done:
        POP     EDI
        POP     ESI
        RET

@@trunc:
{       CL = maxlen     }

        MOV     AL,CL           { AL = final length = maxlen                    }
        SUB     CL,[EDI]        { CL = length to copy = maxlen - destlen        }
        JBE     @@done
        MOV     DL,CL
        JMP     @@cont
end;
{$IFEND}

{$IF not defined(CPU386)}
function _Copy(const S: ShortString; Index, Count: Integer): ShortString;
var
  Len, I: Integer;
begin
  Len := Byte(S[0]);
  if Len = 0 then
    Byte(Result[0]) := 0
  else
  begin
    if Index <= 0 then Index := 1
    else if Index > Len then Count := 0;
    Len := Len - Index + 1;
    if Count < 0 then Count := 0
    else if Count > Len then Count := Len;
    Byte(Result[0]) := Count;
    for I := 1 to Count do
      Result[I] := S[Index + I - 1];
  end;
end;
{$ELSE}


procedure       _Copy{ s : ShortString; index, count : Integer ) : ShortString};
asm
{     ->EAX     Source string                   }
{       EDX     index                           }
{       ECX     count                           }
{       [ESP+4] Pointer to result string        }

        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,[ESP+8+4]

        XOR     EAX,EAX
        OR      AL,[ESI]
        JZ      @@srcEmpty

{       limit index to satisfy 1 <= index <= Length(src) }

        TEST    EDX,EDX
        JLE     @@smallInx
        CMP     EDX,EAX
        JG      @@bigInx
@@cont1:

{       limit count to satisfy 0 <= count <= Length(src) - index + 1    }

        SUB     EAX,EDX { calculate Length(src) - index + 1     }
        INC     EAX
        TEST    ECX,ECX
        JL      @@smallCount
        CMP     ECX,EAX
        JG      @@bigCount
@@cont2:

        ADD     ESI,EDX

        MOV     [EDI],CL
        INC     EDI
        REP     MOVSB
        JMP     @@exit

@@smallInx:
        MOV     EDX,1
        JMP     @@cont1
@@bigInx:
{       MOV     EDX,EAX
        JMP     @@cont1 }
@@smallCount:
        XOR     ECX,ECX
        JMP     @@cont2
@@bigCount:
        MOV     ECX,EAX
        JMP     @@cont2
@@srcEmpty:
        MOV     [EDI],AL
@@exit:
        POP     EDI
        POP     ESI
        RET 4
end;
{$IFEND}

{$IF not defined(CPU386)}
procedure _Delete(var S: OpenString; Index, Count: Integer);
var
  Len, TailLen, I: Integer;
begin
  Len := Byte(S[0]);
  if (Index >= 1) and (Index <= Len) then
  begin
    if Count > 0 then
    begin
      TailLen := Len - Index + 1;
      if Count > TailLen then Count := TailLen;
      Byte(S[0]) := Len - Count;
      for I := 1 to TailLen - Count do
        S[Index + I - 1] := S[Index + Count + I - 1];
    end;
  end;
end;
{$ELSE}


procedure       _Delete{ var s : openstring; index, count : Integer };
asm
{     ->EAX     Pointer to s    }
{       EDX     index           }
{       ECX     count           }

        PUSH    ESI
        PUSH    EDI

        MOV     EDI,EAX

        XOR     EAX,EAX
        MOV     AL,[EDI]

{       if index not in [1 .. Length(s)] do nothing     }

        TEST    EDX,EDX
        JLE     @@exit
        CMP     EDX,EAX
        JG      @@exit

{       limit count to [0 .. Length(s) - index + 1]     }

        TEST    ECX,ECX
        JLE     @@exit
        SUB     EAX,EDX         { calculate Length(s) - index + 1       }
        INC     EAX
        CMP     ECX,EAX
        JLE     @@1
        MOV     ECX,EAX
@@1:
        SUB     [EDI],CL        { reduce Length(s) by count                     }
        ADD     EDI,EDX         { point EDI to first char to be deleted }
        LEA     ESI,[EDI+ECX]   { point ESI to first char to be preserved       }
        SUB     EAX,ECX         { #chars = Length(s) - index + 1 - count        }
        MOV     ECX,EAX

        REP     MOVSB

@@exit:
        POP     EDI
        POP     ESI
end;
{$IFEND}

{$IF not defined(CPU386)}
procedure _Insert(const Source: ShortString; var S: OpenString; Index: Integer);
var
  Len: Integer;
  I: Integer;
  Len1, Len2, Len3: Integer;
begin
  Len := Byte(S[0]);
  if Index <= 0 then Index := 1
  else if Index > Len + 1 then Index := Len + 1;

  Len1 := Index - 1;
  Len2 := Byte(Source[0]);
  Len3 := Len - Len1;

  if Len1 + Len2 + Len3 > High(S) then
  begin
    if Len1 + Len2 > High(S) then
    begin
      Len3 := 0;
      Len2 := High(S) - Len1;
    end
    else
      Len3 := High(S) - Len1 - Len2;
  end;

  Byte(S[0]) := Len1 + Len2 + Len3;

  if Len2 > 0 then
  begin
    for I := Len3 downto 1 do
      S[Len1 + Len2 + I] := S[Len1 + I];
    for I := 1 to Len2 do
      S[Len1 + I] := Source[I];
  end;
end;
{$ELSE}


procedure       _Insert{ source : ShortString; var s : openstring; index : Integer };
asm
        {     ->EAX     Pointer to source string        }
        {       EDX     Pointer to destination string   }
        {       ECX     Length of destination string    }
        {       [ESP+4] Index                           }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    ECX
        MOV     ECX,[ESP+16+4]
        SUB     ESP,512         { VAR buf: ARRAY [0..511] of Char       }

        MOV     EBX,EDX         { save pointer to s for later   }
        MOV     ESI,EDX

        XOR     EDX,EDX
        MOV     DL,[ESI]
        INC     ESI

{       limit index to [1 .. Length(s)+1]       }

        INC     EDX
        TEST    ECX,ECX
        JLE     @@smallInx
        CMP     ECX,EDX
        JG      @@bigInx
@@cont1:
        DEC     EDX             { EDX = Length(s)               }
                                { EAX = Pointer to src          }
                                { ESI = EBX = Pointer to s      }
                                { ECX = Index                   }

{       copy index-1 chars from s to buf        }

        MOV     EDI,ESP
        DEC     ECX
        SUB     EDX,ECX         { EDX = remaining length of s   }
        REP     MOVSB

{       copy Length(src) chars from src to buf  }

        XCHG    EAX,ESI         { save pointer into s, point ESI to src         }
        MOV     CL,[ESI]        { ECX = Length(src) (ECX was zero after rep)    }
        INC     ESI
        REP     MOVSB

{       copy remaining chars of s to buf        }

        MOV     ESI,EAX         { restore pointer into s                }
        MOV     ECX,EDX         { copy remaining bytes of s             }
        REP     MOVSB

{       calculate total chars in buf    }

        SUB     EDI,ESP         { length = bufPtr - buf         }
        MOV     ECX,[ESP+512]   { ECX = Min(length, destLength) }
{       MOV     ECX,[EBP-16]   }{ ECX = Min(length, destLength) }
        CMP     ECX,EDI
        JB      @@1
        MOV     ECX,EDI
@@1:
        MOV     EDI,EBX         { Point EDI to s                }
        MOV     ESI,ESP         { Point ESI to buf              }
        MOV     [EDI],CL        { Store length in s             }
        INC     EDI
        REP     MOVSB           { Copy length chars to s        }
        JMP     @@exit

@@smallInx:
        MOV     ECX,1
        JMP     @@cont1
@@bigInx:
        MOV     ECX,EDX
        JMP     @@cont1

@@exit:
        ADD     ESP,512+4
        POP     EDI
        POP     ESI
        POP     EBX
        RET     4
end;
{$IFEND}

// Don't use var param here - var ShortString is an open string param, which
// passes the ptr in EAX and the string's declared buffer length in EDX.
// Compiler codegen expects only two params for this call - ptr and newlength
procedure       _SetLength(s: PShortString; newLength: Byte);
begin
  Byte(s^[0]) := newLength;   // should also fill new space
end;

procedure       _SetString(s: PShortString; buffer: PAnsiChar; len: Byte);
begin
  Byte(s^[0]) := len;
  if buffer <> nil then
    Move(buffer^, s^[1], len);
end;


{ ----------------------------------------------------- }
{       Compiler helper for AnsiString support          }
{ ----------------------------------------------------- }




procedure _LStrFromChar(var Dest: AnsiString; Source: AnsiChar; CodePage: Word);
{$IF not defined(CPU386)}
begin
  _LStrFromPCharLen(Dest, @Source, 1, CodePage);
end;
{$ELSE}
asm
        { ->    EAX     pointer to dest         }
        {       DL      source ANSI character   }
        {       ECX     CodePage                }
{$IFDEF ALIGN_STACK}
        SUB     ESP,4
{$ENDIF ALIGN_STACK}
        PUSH    EDX
        MOV     EDX,ESP
        PUSH    ECX
        MOV     ECX,1
        CALL    _LStrFromPCharLen
{$IFDEF ALIGN_STACK}
        ADD     ESP,8
{$ELSE !ALIGN_STACK}
        POP     EDX
{$ENDIF !ALIGN_STACK}
end;
{$IFEND}




procedure _LStrFromWChar(var Dest: AnsiString; Source: WideChar; CodePage: Word);
{$IF not defined(CPU386)}
begin
  _LStrFromPWCharLen(Dest, @Source, 1, CodePage);
end;
{$ELSE}
asm
        { ->    EAX     pointer to dest         }
        {       DX      source wide character   }
        {       ECX     CodePage                }
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EDX
        MOV     EDX,ESP
        PUSH    ECX
        MOV     ECX,1
        CALL    _LStrFromPWCharLen
        POP     EDX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
end;
{$IFEND}




procedure _LStrFromPChar(var Dest: AnsiString; Source: PAnsiChar; CodePage: Word);
{$IF not defined(CPU386)}
var
  Len: Integer;
  P: PAnsiChar;
begin
  Len := 0;
  if Source <> nil then
  begin
    P := Source;
    while P^ <> #0 do Inc(P);
    Len := P - Source;
  end;
  _LStrFromPCharLen(Dest, Source, Len, CodePage);
end;
{$ELSE}
asm
        { ->    EAX     pointer to dest                 }
        {       EDX     pointer to ANSI characters      }
        {       ECX     CodePage                        }
{$IFDEF ALIGN_STACK}
        SUB     ESP,8
        PUSH    ECX
{$ELSE ALIGN_STACK}
        PUSH    [ESP]
        MOV     [ESP+4],ECX
{$ENDIF ALIGN_STACK}
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CL,[EDX+0]
        JE      @@4
        CMP     CL,[EDX+1]
        JE      @@3
        CMP     CL,[EDX+2]
        JE      @@2
        CMP     CL,[EDX+3]
        JE      @@1
        ADD     EDX,4
        JMP     @@0
@@1:    INC     EDX
@@2:    INC     EDX
@@3:    INC     EDX
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
@@5:
{$IFDEF ALIGN_STACK}
        CALL    _LStrFromPCharLen
        ADD     ESP,8
{$ELSE ALIGN_STACK}
        JMP     _LStrFromPCharLen
{$ENDIF ALIGN_STACK}
end;
{$IFEND}




procedure _LStrFromPWChar(var Dest: AnsiString; Source: PWideChar; CodePage: Word);
{$IF not defined(CPU386)}
var
  Len: Integer;
  P: PWideChar;
begin
  Len := 0;
  if Source <> nil then
  begin
    P := Source;
    while P^ <> #0 do Inc(P);
    Len := P - Source;
  end;
  _LStrFromPWCharLen(Dest, Source, Len, CodePage);
end;
{$ELSE}
asm
        { ->    EAX     pointer to dest                 }
        {       EDX     pointer to wide characters      }
        {       ECX     CodePage                        }
{$IFDEF ALIGN_STACK}
        SUB     ESP,8
        PUSH    ECX
{$ELSE ALIGN_STACK}
        PUSH    [ESP]
        MOV     [ESP+4],ECX
{$ENDIF ALIGN_STACK}
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CX,[EDX+0]
        JE      @@4
        CMP     CX,[EDX+2]
        JE      @@3
        CMP     CX,[EDX+4]
        JE      @@2
        CMP     CX,[EDX+6]
        JE      @@1
        ADD     EDX,8
        JMP     @@0
@@1:    ADD     EDX,2
@@2:    ADD     EDX,2
@@3:    ADD     EDX,2
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
        SHR     ECX,1
@@5:
{$IFDEF ALIGN_STACK}
        CALL    _LStrFromPWCharLen
        ADD     ESP,8
{$ELSE ALIGN_STACK}
        JMP     _LStrFromPWCharLen
{$ENDIF ALIGN_STACK}
end;
{$IFEND}




procedure _LStrFromString(var Dest: AnsiString; const Source: ShortString; CodePage: Word);
{$IF not defined(CPU386)}
begin
  _LStrFromPCharLen(Dest, @Source[1], Byte(Source[0]), CodePage);
end;
{$ELSE}
asm
        { ->    EAX     pointer to dest         }
        {       EDX     pointer to ShortString  }
        {       ECX     CodePage                }
{$IFDEF ALIGN_STACK}
        SUB     ESP,8
        PUSH    ECX
{$ELSE ALIGN_STACK}
        PUSH    [ESP]
        MOV     [ESP+4],ECX
{$ENDIF ALIGN_STACK}
        XOR     ECX,ECX
        MOV     CL,[EDX]
        INC     EDX
{$IFDEF ALIGN_STACK}
        CALL    _LStrFromPCharLen
        ADD     ESP,8
{$ELSE ALIGN_STACK}
        JMP     _LStrFromPCharLen
{$ENDIF ALIGN_STACK}
end;
{$IFEND}




procedure _LStrFromArray(var Dest: AnsiString; Source: PAnsiChar; Length: Integer; CodePage: Word);
{$IF not defined(CPU386)}
var
  P: PAnsiChar;
begin
  P := Source;
  while (Length > 0) and (P^ <> #0) do
  begin
    Dec(Length);
    Inc(P);
  end;
  Length := P - Source;
  _LStrFromPCharLen(Dest, Source, Length, CodePage);
end;
{$ELSE}
asm
        { ->    EAX     pointer to dest                     }
        {       EDX     pointer to source ANSI characters   }
        {       ECX     number of characters of src         }
        {       [ESP+0] caller EBP                          }
        {       [ESP+4] return address                      }
        {       [ESP+8] CodePage                            }
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASB
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        POP     EBP
        JMP     _LStrFromPCharLen
end;
{$IFEND}




procedure _LStrFromWArray(var Dest: AnsiString; Source: PWideChar; Length: Integer; CodePage: Word);
{$IF not defined(CPU386)}
var
  P: PWideChar;
begin
  P := Source;
  while (Length > 0) and (P^ <> #0) do
  begin
    Dec(Length);
    Inc(P);
  end;
  Length := P - Source;
  _LStrFromPWCharLen(Dest, Source, Length, CodePage);
end;
{$ELSE}
asm
        { ->    EAX     pointer to dest                     }
        {       EDX     pointer to source wide characters   }
        {       ECX     number of characters of src         }
        {       [ESP+0] caller EBP                          }
        {       [ESP+4] return address                      }
        {       [ESP+8] CodePage                            }
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASW
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        POP     EBP
        JMP     _LStrFromPWCharLen
end;
{$IFEND}


procedure _LStrFromWStr(var Dest: AnsiString; const Source: WideString; CodePage: Word);
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
  _LStrFromUStr(Dest, UnicodeString(Pointer(Source)), CodePage);
end;
{$ELSE}
asm
        JMP     _LStrFromUStr
end;
{$ENDIF !PUREPASCAL}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
var
  Len: Integer;
begin
  Len := 0;
  if Pointer(Source) <> nil then
    Len := __StringLength(Source);
  _LStrFromPWCharLen(Dest, PWideChar(Pointer(Source)), Len, CodePage);
end;
{$ELSE}
asm
        { ->    EAX     pointer to dest                 }
        {       EDX     pointer to WideString data      }
        {       ECX     CodePage                        }
        PUSH    [ESP]
        MOV     [ESP+4],ECX
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@1
        MOV     ECX,[EDX-4]
        SHR     ECX,1
@@1:
        JMP     _LStrFromPWCharLen
end;
{$ENDIF !PUREPASCAL}
{$ENDIF}

{$IF not defined(CPU386)}

procedure _LStrToString(Dest: PShortString; const Source: AnsiString; MaxLen: Integer);
var
  Len: Integer;
begin
  if (Pointer(Source) = nil) or (__StringLength(Source) = 0) then
    Byte(Dest^[0]) := 0
  else
  begin
    Len := __StringLength(Source);
    if Len > MaxLen then Len := MaxLen;
    Byte(Dest^[0]) := Len;
    Move(Source[1], Dest^[1], Len);
  end;
end;
{$ELSE}


procedure _LStrToString{(var Dest: ShortString; const Source: AnsiString; MaxLen: Integer)};
asm
        { ->    EAX pointer to result   }
        {       EDX AnsiString s        }
        {       ECX length of result    }

        PUSH    EBX
        TEST    EDX,EDX
        JE      @@empty
        MOV     EBX,[EDX-skew].StrRec.length
        TEST    EBX,EBX
        JE      @@empty

        CMP     ECX,EBX
        JL      @@truncate
        MOV     ECX,EBX
@@truncate:
        MOV     [EAX],CL
        INC     EAX

        XCHG    EAX,EDX
{$IFDEF ALIGN_STACK}
        SUB     ESP,8
{$ENDIF ALIGN_STACK}
        CALL    Move
{$IFDEF ALIGN_STACK}
        ADD     ESP,8
{$ENDIF ALIGN_STACK}

        JMP     @@exit

@@empty:
        MOV     byte ptr [EAX],0

@@exit:
        POP     EBX
end;
{$IFEND}

{$IF not defined(CPU386)}

procedure _LStrCat(var Dest: AnsiString; const Source: AnsiString);
var
  CodePage: Word;
  L1, L2, Len: Cardinal;
  Src: Pointer;
begin
  if Pointer(Source) <> nil then
  begin
    if Pointer(Dest) = nil then
      _LStrAsg(Dest, Source)
    else
    begin
      Src := Pointer(Source);
      if Pointer(Dest) = Pointer(Source) then
      begin
        L1 := __StringLength(Dest);
        L2 := L1;
      end else
      begin
        L1 := __StringLength(Dest);
        L2 := __StringLength(Src);
      end;
      Len := L1 + L2;
      if (((L1 and L2) or ((not Len) and (L1 or L2))) and $80000000) <> 0 then _IntOver;
      _LStrSetLength(Dest, Len, __StringCodePage(Dest));
      Move(PAnsiChar(Src)^, PAnsiChar(Dest)[L1], L2);
    end;
  end;
end;
{$ELSE}


procedure       _LStrCat{var dest: AnsiString; source: AnsiString};
asm
        { ->    EAX     pointer to dest }
        {       EDX     source          }

        TEST    EDX,EDX
        JE      @@exit

        MOV     ECX,[EAX]
        TEST    ECX,ECX
        JE      _LStrAsg

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX
        CMP     ESI,ECX
        MOV     EDI,[ECX-skew].StrRec.length

        MOV     EDX,[ESI-skew].StrRec.length
        ADD     EDX,EDI
        JO      @@lengthOverflow
        CMP     ESI,ECX
        JE      @@appendSelf

        MOVZX   ECX,[ECX-skew].StrRec.codePage
        CALL    _LStrSetLength
        MOV     EAX,ESI
        MOV     ECX,[ESI-skew].StrRec.length

@@appendStr:
        MOV     EDX,[EBX]
        ADD     EDX,EDI
        CALL    Move
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@appendSelf:
        MOVZX   ECX,[ECX-skew].StrRec.codePage
        CALL    _LStrSetLength
        MOV     EAX,[EBX]
        MOV     ECX,EDI
        JMP     @@appendStr

@@lengthOverflow:
{$IFDEF ALIGN_STACK}
        POP     EDI
        POP     ESI
        POP     EBX
{$ENDIF ALIGN_STACK}
        JMP     _IntOver

@@exit:
end;
{$IFEND}

{$IF not defined(CPU386)}

procedure _LStrCat3(var Dest:AnsiString; const Source1, Source2: AnsiString);
var
  CodePage: Word;
  S1, S2: Pointer;
  Temp: Pointer;
  L1, L2, Len: Cardinal;
begin
  if Pointer(Source1) = nil then
    _LStrAsg(Dest, Source2)
  else if Pointer(Source2) = nil then
    _LStrAsg(Dest, Source1)
  else
  begin
    if Pointer(Dest) = Pointer(Source1) then
      _LStrCat(Dest, Source2)
    else if Pointer(Dest) = Pointer(Source2) then
    begin
      L1 := __StringLength(Source1);
      L2 := __StringLength(Source2);
      Len := L1 + L2;
      if (((L1 and L2) or ((not Len) and (L1 or L2))) and $80000000) <> 0 then _IntOver;
      CodePage := __StringCodePage(Source2);
      Temp := _NewAnsiString(Len, CodePage);
      Move(PAnsiChar(Source1)^, PAnsiChar(Temp)[0], L1);
      Move(PAnsiChar(Source2)^, PAnsiChar(Temp)[L1], L2);
      _LStrClr(Dest);
      Pointer(Dest) := Temp;
    end
    else
    begin
      _LStrAsg(Dest, Source1);
      _LStrCat(Dest, Source2);
    end;
  end;
end;
{$ELSE}



procedure       _LStrCat3{var dest:AnsiString; source1: AnsiString; source2: AnsiString};
asm
        {     ->EAX = Pointer to dest   }
        {       EDX = source1           }
        {       ECX = source2           }

        TEST    EDX,EDX
        JE      @@assignSource2

        TEST    ECX,ECX
        JE      _LStrAsg

        CMP     EDX,[EAX]
        JE      @@appendToDest

        CMP     ECX,[EAX]
        JE      @@theHardWay

{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    ECX
        CALL    _LStrAsg

        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        JMP     _LStrCat

@@theHardWay: // s(*EAX,ECX) := source1(EDX) + s(ECX)

        PUSH    EDI

        MOV     EDI,[EDX-skew].StrRec.length  // EDI := Length(source1) + Length(source2)
        ADD     EDI,[ECX-skew].StrRec.length
        JO      @@overflow

        PUSH    EBX
        PUSH    ESI
        PUSH    EAX
        MOV     EBX,EDX   // EBX : source1
        MOV     ESI,ECX   // ESI : source2

{$IFDEF ALIGN_STACK}
        SUB     ESP,12
{$ENDIF ALIGN_STACK}
        MOV     EAX,EDI   // EAX := Final length
        MOVZX   EDX,[ESI-skew].StrRec.codePage // use source2's codepage
        CALL    _NewAnsiString
        MOV     EDI,EAX

        MOV     EDX,EDI  //Move(source1, temp[0], len(source1))
        MOV     EAX,EBX
        MOV     ECX,[EBX-skew].StrRec.length
        CALL    Move

        MOV     EDX,EDI  //Move(source2, temp[len(source1)], len(source2))
        MOV     EAX,ESI
        MOV     ECX,[ESI-skew].StrRec.length
        ADD     EDX,[EBX-skew].StrRec.length
        CALL    Move
{$IFDEF ALIGN_STACK}
        ADD     ESP,12
{$ENDIF ALIGN_STACK}

        POP     EAX
        MOV     EDX,EDI
        TEST    EDI,EDI
        JE      @@skip
        DEC     [EDI-skew].StrRec.refCnt    // EDI = local temp str - pass this reference to the caller
@@skip:
        CALL    _LStrAsg

        POP     ESI
        POP     EBX
        POP     EDI

        JMP     @@exit

@@assignSource2:
        MOV     EDX,ECX
        JMP     _LStrAsg

@@appendToDest:
        MOV     EDX,ECX
        JMP     _LStrCat

@@overflow:
        POP     EDI
        JMP     _IntOver

@@exit:
end;
{$IFEND}

{$IF not defined(CPU386)}
procedure InternalLStrCatN(var Dest: AnsiString; ArgCnt: Integer; Strs: PAnsiString);
var
  CodePage: Word;
  I, J, Start: Integer;
  Len, L: Integer;
  P: Pointer;
  NewDest: Pointer;
  Appending: Boolean;
begin
  CodePage := 0;
  for I := 0 to ArgCnt - 1 do
  begin
    P := PPointerArray(Strs)[I];
    if P <> nil then
    begin
      CodePage := __StringCodePage(P);
      if CodePage <> 0 then
        Break;
    end;
  end;
  Appending := False;
  Len := 0;
  for I := 0 to ArgCnt - 1 do
  begin
    P := PPointerArray(Strs)[I];
    if P <> nil then
    begin
      if P = Pointer(Dest) then
        Appending := (I = 0);
      Inc(Len, __StringLength(P));
      if Len < 0 then _IntOver;
    end;
  end;
  if Appending then
  begin
    // Dest is non-nil
    L := __StringLength(Dest);
    _LStrSetLength(Dest, Len, CodePage);
    NewDest := Pointer(Dest);
    Start := 1;
  end
  else
  begin
    NewDest := _NewAnsiString(Len, CodePage);
    Start := 0;
    L := 0;
  end;
  for I := Start to ArgCnt - 1 do
  begin
    P := PPointerArray(Strs)[I];
    if P <> nil then
    begin
      Move(P^, PAnsiChar(NewDest)[L], __StringLength(P));
      Inc(L, __StringLength(P));
    end;
  end;
  if not Appending then
  begin
    if Pointer(Dest) <> nil then
      _LStrClr(Dest);
    Pointer(Dest) := NewDest;
  end;
end;
{$IFEND}




{$IF not defined(CPU386)}
procedure _LStrCatN(var Dest: AnsiString; ArgCnt: Integer; const Strs: AnsiString); varargs;
begin
  InternalLStrCatN(Dest, ArgCnt, @Strs);
end;
{$ELSE}
procedure       _LStrCatN{var dest:AnsiString; argCnt: Integer; ...};
asm //StackAligned
        {     ->EAX = Pointer to dest           }
        {       EDX = number of args (>= 3)     }
        {       [EBP+8], [EBP+12], ... crgCnt AnsiString arguments, reverse order }

        PUSH    0                   // Stack - xxxxxxx8 - Save CodePage
        PUSH    EBX                 // Stack - xxxxxxx4
        PUSH    ESI                 // Stack - xxxxxxx0
        PUSH    EDI                 // Stack - xxxxxxxc
        PUSH    EDX                 // Stack - xxxxxxx8
        PUSH    EAX                 // Stack - xxxxxxx4
        PUSH    0                   // Stack - xxxxxxx0 - Local Temp
        MOV     EBX,EDX

        XOR     EDI,EDI
        MOV     ECX,[ESP+EDX*4+7*4] // first arg is furthest out
        TEST    ECX,ECX
        JZ      @@0
        MOVZX   ESI,[ECX-skew].StrRec.codePage
        MOV     [ESP+6*4],ESI      // Save the first arg's code page in case we need to copy
        CMP     [EAX],ECX          // is dest = first arg?
        JNE     @@0
        MOV     EDI,ECX            // EDI nonzero -> potential appendstr case
        MOV     EAX,[ECX-skew].StrRec.length
        DEC     EDX
        JMP     @@loop1
@@0:
        XOR     EAX,EAX
@@loop1:
        MOV     ECX,[ESP+EDX*4+7*4]
        TEST    ECX,ECX
        JE      @@1
        ADD     EAX,[ECX-skew].StrRec.length
        JO      @@overflow

        CMP     [ESP+6*4],0        // Have we already found a valid codepage?
        JNZ     @@hascodepage
        MOVZX   ESI,[ECX-skew].StrRec.codePage // Save the first non-blank arg we find's codepage
        MOV     [ESP+6*4],ESI
@@hascodepage:

        CMP     EDI,ECX          // is dest an arg besides arg1?
        JNE     @@1
        XOR     EDI,EDI          // can't appendstr - dest is multiple args
@@1:
        DEC     EDX
        JNE     @@loop1

@@append:
        TEST    EDI,EDI          // dest is 1st and only 1st arg?
        JZ      @@copy
        MOV     EDX,EAX          // length into EDX
        MOV     EAX,[ESP + 4]    // ptr to str into EAX
        MOV     ESI,[EDI-skew].StrRec.Length  // save old size before realloc
        MOVZX   ECX,[EDI-Skew].StrRec.codePage
        CALL    _LStrSetLength
        MOV     EDI,[ESP + 4]        // append other strs to dest
        MOV     EAX,[EDI]        // Stack - xxxxxxx0
        MOV     [ESP],EAX
        ADD     ESI,[EDI]        // ESI = end of old string
        DEC     EBX
        JMP     @@loop2

@@copy:
        MOV     EDX,[ESP+6*4]
        CALL    _NewAnsiString
        MOV     [ESP],EAX       // Stack - xxxxxxx0
        MOV     ESI,EAX

@@loop2:
        MOV     EAX,[ESP+EBX*4+7*4]
        MOV     EDX,ESI
        TEST    EAX,EAX
        JE      @@2
        MOV     ECX,[EAX-skew].StrRec.length
        ADD     ESI,ECX
        CALL    Move
@@2:
        DEC     EBX
        JNE     @@loop2

        MOV     EDX,[ESP]
        MOV     EAX,[ESP+4]         // Stack - xxxxxxx0
        TEST    EDI,EDI
        JNZ     @@exit

        TEST    EDX,EDX
        JE      @@skip
        DEC     [EDX-skew].StrRec.refCnt   // EDX = local temp str
@@skip:
        CALL    _LStrAsg

@@exit:
        ADD     ESP,8           // Stack - xxxxxxx8 - Clean Local Temp & Saved EAX
        POP     EDX             // Stack - xxxxxxxc
        POP     EDI             // Stack - xxxxxxx0
        POP     ESI             // Stack - xxxxxxx4
        POP     EBX             // Stack - xxxxxxx8
        POP     EAX             // Stack - xxxxxxxc - Codepage Temp
        POP     EAX             // Stack - xxxxxxx0 - Return Address
        LEA     ESP,[ESP+EDX*4]
        JMP     EAX // Unbalanced CALL/RET means clobbered branch prediction.
                    // Should fix codegen and have caller pop arguments, like cdecl.

@@overflow:
        JMP     _IntOver
end;
{$IFEND}

{$IF not defined(CPU386)}

// Returns 0 : Left = Right
//     minus : Left < Right
//      plus : Left > Right
function _LStrCmp(const Left, Right: AnsiString): Integer;
var
  Len, LLen, RLen: Integer;
  LPtr, RPtr: PAnsiChar;
  L, R: Pointer;
begin
  if Pointer(Left) = Pointer(Right) then
    Result := 0
  else if Pointer(Left) = nil then
    Result := 0 - _LStrLen(Right)
  else if Pointer(Right) = nil then
    Result := _LStrLen(Left);
  begin

    L := Pointer(Left);
    R := Pointer(Right);
    LLen := __StringLength(AnsiString(L));
    RLen := __StringLength(AnsiString(R));
    Len := LLen;
    if Len > RLen then Len := RLen;
    LPtr := PAnsiChar(L);
    RPtr := PAnsiChar(R);
    while Len > 0 do
    begin
      Result := Ord(LPtr^) - Ord(RPtr^);
      if Result <> 0 then break;
      Dec(Len);
      if Len = 0 then break;
      Result := Ord(LPtr[1]) - Ord(RPtr[1]);
      if Result <> 0 then break;
      Inc(LPtr, 2);
      Inc(RPtr, 2);
      Dec(Len);
    end;
    if Len = 0 then
      Result := LLen - RLen;
  end;
end;
{$ELSE}


{Original code by Pierre le Riche. Licensed under the CodeGear license terms.}
procedure _LStrCmp{left: AnsiString; right: AnsiString};
asm
  {On entry:
     eax = @Left[1]
     edx = @Right[1]
   On exit:
     Result in flags:
       CF = 1 if Left < Right, CF = 0 otherwise
       ZF = 1 if Left = Right, ZF = 0 otherwise}

  CMP   EAX, EDX   // Do S1 and S2 point to the same string data?
  JE    @DoneNoPop

  TEST  EAX, EDX   // Is one of the two string pointers perhaps nil?
  JZ    @PossibleNilString
@BothStringsNonNil:
  {Compare the first character. (There has to be a trailing #0, so this
   comparison is safe). In "random" string compares this can save significant
   CPU time.}
  MOVZX ECX, BYTE PTR [EAX]
  SUB   CL, [EDX]
  JNE   @DoneNoPop

  PUSH  EBX             // Save ebx
  MOV   EBX, [EAX - 4]  // Set ebx = length(S1)
  SUB   EBX, [EDX - 4]  // Set ebx = length(S1) - length(S2)
  PUSH  EBX             // Save the length difference on the stack

  ADC   ECX, -1 // Set ecx = 0 if length(S1) <= length(S2), $ffffffff otherwise
  AND   ECX, EBX        // Set ecx = - min(length(S1), length(S2))

  SUB   ECX, [EAX - 4]
  {Adjust the pointers to be negative offset based}
  SUB   EAX, ECX
  SUB   EDX, ECX
@CompareLoop:
  {Compare four bytes per cycle. (The start of string data is at least DWord
   aligned, so this is safe.)}
  MOV   EBX, [EAX + ECX]
  XOR   EBX, [EDX + ECX]
  JNZ   @Mismatch

  ADD   ECX, 4   // Next four bytes
  JS    @CompareLoop

@MatchUpToLength: // All characters match up to the compare length
  POP   EAX       // Restore the string length difference to eax
  ADD   EAX, EAX  // Set the flags according to the length difference
  POP   EBX       // Restore ebx and return
@DoneNoPop:
  RET

@Mismatch:
  BSF   EBX, EBX // Find the byte index that mismatched
  SHR   EBX, 3

  ADD   ECX, EBX //   Is the mismatch beyond the compare length?
  JNS   @MatchUpToLength

  MOV   AL, [EAX + ECX] // Compare the mismatched byte, setting the flags
  CMP   AL, [EDX + ECX]

  POP   EBX      // Pop the length difference, restore ebx and return
  POP   EBX
  RET
@PossibleNilString:
  {There is a good probability that one of the strings are nil (but not both)}
  TEST  EAX, EAX
  JZ    @FirstStringNil
  TEST  EDX, EDX
  JNZ   @BothStringsNonNil

  CMP   [EAX - 4], EDX // S2 is nil - compare lengths of the strings
  RET
@FirstStringNil:
  CMP   EAX, [EDX - 4] // S1 is nil - compare lengths of the strings
end;
{$IFEND}

{$IF not defined(CPU386)}

function _LStrEqual(const Left, Right: AnsiString): Integer;
begin
  Result := _LStrCmp(Left, Right);
end;
{$ELSE}


{Original code by Pierre le Riche. Licensed under the CodeGear license terms.}
procedure _LStrEqual{const Left, Right: AnsiString};
asm
  {On entry:
     eax = @Left[1]
     edx = @Right[1]
   On exit:
     Result in flags:
       ZF = 1 if Left = Right, ZF = 0 otherwise}

        CMP   EAX, EDX  //Do Left and Right point to the same string data?
        JE    @CompareDoneNoPop

        TEST  EAX, EDX  //Is one of the two string pointers perhaps nil?
        JZ    @PossibleNilString
@BothStringsNonNil:
        MOV   ECX, [EAX - 4] //Compare lengths
        CMP   ECX, [EDX - 4]
        JNE   @CompareDoneNoPop

        PUSH  EBX       // Save ebx
        {Get pointers to the 4th last bytes in the strings}
        LEA   EDX, [EDX + ECX - 4]
        LEA   EBX, [EAX + ECX - 4]
        NEG   ECX       // Negate the loop counter
        {Compare the last four bytes. If the string length is less
         than four bytes then part of the length field is compared
         again - no harm done.}
        MOV   EAX, [EBX]
        CMP   EAX, [EDX]
        JNE   @CompareDonePop
@CompareLoop:
        ADD   ECX, 4 // Next four bytes
        JNS   @Match
        {Compare four bytes per iteration}
        MOV   EAX, [EBX + ECX]
        CMP   EAX, [EDX + ECX]
        JE    @CompareLoop
@CompareDonePop:
        POP   EBX
@CompareDoneNoPop:
        RET
@Match:
        XOR   EAX, EAX // Strings match - set the zero flag
        POP   EBX
        RET
@PossibleNilString:
        {There is a good probability that one of the strings are nil
         (but not both)}
        TEST  EAX, EAX
        JZ    @FirstStringNil
        TEST  EDX, EDX
        JNZ   @BothStringsNonNil
        {Right is nil - compare lengths of the strings}
        CMP   [EAX - 4], EDX
        RET
@FirstStringNil:
        {Left is nil - compare lengths of the strings}
        CMP   EAX, [EDX - 4]
        RET
end;
{$IFEND}

type
  TEmptyString = packed record
    Rec: StrRec;
    Nul: Word;
  end;

const
  // Using initialized constant to be sure of alignment.
  // Not as read-only as code segment, but code entry points
  // have no alignment guarantees.
  EmptyStringA: TEmptyString = (
    Rec: (
      codePage: Word($FFFF);
      elemSize: 1;
      refCnt: -1;
      length: 0);
    Nul: 0);





function _LStrToPChar(const S: AnsiString): PAnsiChar;
begin
  if Pointer(s) = nil then
    Result := @EmptyStringA.Nul
  else
    Result := Pointer(s);
end;

{$IF not defined(CPU386)}

function _LStrCopy(const Str: AnsiString; Index, Count: Integer): AnsiString;
var
  SrcLen, CopyLen: Integer;
  S: Pointer;
begin
  _LStrClr(Result);
  if Pointer(Str) <> nil then
  begin
    S := Pointer(Str);

    SrcLen := __StringLength(S);
    if SrcLen <> 0 then
    begin
      if Index <= 0 then Index := 1;
      if Index <= SrcLen then
      begin
        CopyLen := SrcLen - (Index - 1);
        if Count >= 0 then
        begin
          if CopyLen > Count then CopyLen := Count;
          _LStrFromPCharLen(Result, @PAnsiChar(S)[Index - 1], CopyLen, __StringCodePage(S));
        end;
      end;
    end;

    if S <> Pointer(Str) then
      _LStrClr(S);
  end;
end;
{$ELSE}



procedure _LStrCopy{ const Str : AnsiString; Index, Count: Integer) : AnsiString};
asm
        {     ->EAX     Source string                   }
        {       EDX     index                           }
        {       ECX     count                           }
        {       [ESP+4] Pointer to result string        }

        PUSH    EBX

        TEST    EAX,EAX
        JE      @@srcEmpty

        MOV     EBX,[EAX-skew].StrRec.length
        TEST    EBX,EBX
        JE      @@srcEmpty

{       make index 0-based and limit to 0 <= index < Length(src) }

        DEC     EDX
        JL      @@smallInx
        CMP     EDX,EBX
        JGE     @@bigInx

@@cont1:

{       limit count to satisfy 0 <= count <= Length(src) - index        }

        SUB     EBX,EDX { calculate Length(src) - index }
        TEST    ECX,ECX
        JL      @@smallCount
        CMP     ECX,EBX
        JG      @@bigCount

@@cont2:
        ADD     EDX,EAX
        MOVZX   EAX,[EAX-skew].StrRec.codePage
{$IFDEF ALIGN_STACK}
        SUB     ESP,4
        PUSH    EAX
        MOV     EAX, [ESP+16]
{$ELSE !ALIGN_STACK}
        PUSH    EAX
        MOV     EAX, [ESP+12]
{$ENDIF ALIGN_STACK}
        CALL    _LStrFromPCharLen
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        JMP     @@exit

@@smallInx:
        XOR     EDX,EDX
        JMP     @@cont1
@@bigCount:
        MOV     ECX,EBX
        JMP     @@cont2
@@bigInx:
@@smallCount:
@@srcEmpty:
        MOV     EAX,[ESP+4+4]
{$IFDEF ALIGN_STACK}
        SUB     ESP,8
{$ENDIF ALIGN_STACK}
        CALL    _LStrClr
{$IFDEF ALIGN_STACK}
        ADD     ESP,8
{$ENDIF ALIGN_STACK}
@@exit:
        POP     EBX
        RET     4
end;
{$IFEND}

{$IF not defined(CPU386)}
procedure _LStrDelete(var S: AnsiString; Index, Count: Integer);
var
  Len, TailLen: Integer;
begin
  InternalUniqueStringA(S);
  if Pointer(S) <> nil then
  begin
    Len := __StringLength(S);
    if (Index >= 1) and (Index <= Len) then
    begin
      if Count > 0 then
      begin
        TailLen := Len - Index + 1;
        if Count > TailLen then Count := TailLen;
        Move(S[Index + Count], S[Index], TailLen - Count);
        _LStrSetLength(S, Len - Count, __StringCodePage(S));
      end;
    end;
  end;
end;
{$ELSE}


procedure       _LStrDelete{ var s : AnsiString; index, count : Integer };
asm
        {     ->EAX     Pointer to s    }
        {       EDX     index           }
        {       ECX     count           }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        CALL    InternalUniqueStringA

        MOV     EDX,[EBX]
        TEST    EDX,EDX         { source already empty: nothing to do   }
        JE      @@exit

        MOV     ECX,[EDX-skew].StrRec.length

{       make index 0-based, if not in [0 .. Length(s)-1] do nothing     }

        DEC     ESI
        JL      @@exit
        CMP     ESI,ECX
        JGE     @@exit

{       limit count to [0 .. Length(s) - index] }

        TEST    EDI,EDI
        JLE     @@exit
        SUB     ECX,ESI         { ECX = Length(s) - index       }
        CMP     EDI,ECX
        JLE     @@1
        MOV     EDI,ECX
@@1:

{       move length - index - count characters from s+index+count to s+index }

        SUB     ECX,EDI         { ECX = Length(s) - index - count       }
        ADD     EDX,ESI         { EDX = s+index                 }
        LEA     EAX,[EDX+EDI]   { EAX = s+index+count           }
        CALL    Move

{       set length(s) to length(s) - count      }

        MOV     EDX,[EBX]
        MOV     EAX,EBX
        MOVZX   ECX,[EDX-skew].StrRec.codePage
        MOV     EDX,[EDX-skew].StrRec.length
        SUB     EDX,EDI
        CALL    _LStrSetLength

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$IFEND}

{$IF not defined(CPU386)}
procedure _LStrInsert(const Source: AnsiString; var S: AnsiString; Index: Integer);
var
  CodePage: Word;
  Len: Integer;
  Len1, Len2, Len3: Integer;
begin
  if Pointer(Source) <> nil then
  begin
    CodePage := 0;
    Len := 0;
    if Pointer(S) <> nil then
    begin
      Len := __StringLength(S);
      CodePage := __StringCodePage(S);
    end;
    if Index <= 0 then Index := 1
    else if Index > Len + 1 then Index := Len + 1;
    Len1 := Index - 1;
    Len2 := __StringLength(Source);
    Len3 := Len - Len1;
    _LStrSetLength(S, Len + Len2, CodePage);
    if Len2 > 0 then
    begin
      Move(S[Index], S[Index + Len2], Len3);
      Move(Source[1], S[Index], Len2);
    end;
  end;
end;
{$ELSE}


procedure       _LStrInsert{ const source : AnsiString; var s : AnsiString; index : Integer };
asm
        { ->    EAX     source string                   }
        {       EDX     pointer to destination string   }
        {       ECX     index                           }

        TEST    EAX,EAX
        JE      @@nothingToDo

{$IFDEF ALIGN_STACK}
        SUB     ESP,12
{$ENDIF}
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

{       make index 0-based and limit to 0 <= index <= Length(s) }

        MOV     EDX,[EDX]
        PUSH    EDX
        TEST    EDX,EDX
        JE      @@sIsNull
        MOV     EDX,[EDX-skew].StrRec.length
@@sIsNull:
        DEC     EDI
        JGE     @@indexNotLow
        XOR     EDI,EDI
@@indexNotLow:
        CMP     EDI,EDX
        JLE     @@indexNotHigh
        MOV     EDI,EDX
@@indexNotHigh:

        MOV     EBP,[EBX-skew].StrRec.length

{       set length of result to length(source) + length(s)      }

        MOV     EAX,[ESI]
        TEST    EAX,EAX
        JNE     @@DestNotNull
        MOV     EAX,EBX
@@DestNotNull:
        MOVZX   ECX,[EAX-skew].StrRec.codePage
        MOV     EAX,ESI
        ADD     EDX,EBP
        JO      @@overflow
        CALL    _LStrSetLength
        POP     EAX

        CMP     EAX,EBX
        JNE     @@notInsertSelf
        MOV     EBX,[ESI]

@@notInsertSelf:

{       move length(s) - length(source) - index chars from s+index to s+index+length(source) }

        MOV     EAX,[ESI]                       { EAX = s       }
        LEA     EDX,[EDI+EBP]                   { EDX = index + length(source)  }
        MOV     ECX,[EAX-skew].StrRec.length
        SUB     ECX,EDX                         { ECX = length(s) - length(source) - index }
        ADD     EDX,EAX                         { EDX = s + index + length(source)      }
        ADD     EAX,EDI                         { EAX = s + index       }
        CALL    Move

{       copy length(source) chars from source to s+index        }

        MOV     EAX,EBX
        MOV     EDX,[ESI]
        MOV     ECX,EBP
        ADD     EDX,EDI
        CALL    Move

@@exit:
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
{$IFDEF ALIGN_STACK}
        ADD     ESP,12
{$ENDIF}
@@nothingToDo:
        RET

@@overflow:
{$IFDEF ALIGN_STACK}
        SUB     ESP,4
{$ENDIF}
        JMP     _IntOver
end;
{$IFEND}

{
  This one needs to be visible for AnsiString support in C++.
}
{$IF not defined(CPU386)}

function _LStrPos(const SubStr: AnsiString; const S: AnsiString): Integer;
begin
  Result := Pos(RawByteString(Pointer(SubStr)), RawByteString(Pointer(S)));
end;
{$ELSE}


procedure       _LStrPos{ const substr : AnsiString; const s : AnsiString ) : Integer};
asm
{     ->EAX     Pointer to substr               }
{       EDX     Pointer to string               }
{     <-EAX     Position of substr in s or 0    }

        TEST    EAX,EAX
        JE      @@noWork

        TEST    EDX,EDX
        JE      @@stringEmpty

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX                         { Point ESI to substr           }
        MOV     EDI,EDX                         { Point EDI to s                }

        MOV     ECX,[EDI-skew].StrRec.length    { ECX = Length(s)               }

        PUSH    EDI                             { remember s position to calculate index        }

        MOV     EDX,[ESI-skew].StrRec.length    { EDX = Length(substr)          }

        DEC     EDX                             { EDX = Length(substr) - 1              }
        JS      @@fail                          { < 0 ? return 0                        }
        MOV     AL,[ESI]                        { AL = first char of substr             }
        INC     ESI                             { Point ESI to 2'nd char of substr      }

        SUB     ECX,EDX                         { #positions in s to look at    }
                                                { = Length(s) - Length(substr) + 1      }
        JLE     @@fail
@@loop:
        REPNE   SCASB
        JNE     @@fail
        MOV     EBX,ECX                         { save outer loop counter               }
        PUSH    ESI                             { save outer loop substr pointer        }
        PUSH    EDI                             { save outer loop s pointer             }

        MOV     ECX,EDX
        REPE    CMPSB
        POP     EDI                             { restore outer loop s pointer  }
        POP     ESI                             { restore outer loop substr pointer     }
        JE      @@found
        MOV     ECX,EBX                         { restore outer loop counter    }
        JMP     @@loop

@@fail:
        POP     EDX                             { get rid of saved s pointer    }
        XOR     EAX,EAX
        JMP     @@exit

@@stringEmpty:
        XOR     EAX,EAX
        JMP     @@noWork

@@found:
        POP     EDX                             { restore pointer to first char of s    }
        MOV     EAX,EDI                         { EDI points of char after match        }
        SUB     EAX,EDX                         { the difference is the correct index   }
@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
@@noWork:
end;
{$IFEND}

{$IF not defined(CPU386)}

procedure _LStrSetLength(var Str: AnsiString; NewLength: Integer; CodePage: Word);
var
  P: PStrRec;
  Temp: Pointer;
  CopyCount: Integer;
begin
  if newLength <= 0 then
  begin
    _LStrClr(Str);
    Exit;
  end
  else
  begin
    if Pointer(Str) <> nil then
    begin
      if __StringRefCnt(Str) = 1 then
      begin
        P := Pointer(NativeInt(Str) - Sizeof(StrRec));
        _ReallocMem(Pointer(P), NewLength + 1 + SizeOf(StrRec));
        P.length := NewLength;
        Pointer(Str) := Pointer(NativeInt(P) + SizeOf(StrRec));
        PAnsiChar(Str)[NewLength] := #0;
        Exit;
      end;
    end;
    Temp := _NewAnsiString(NewLength, CodePage);
    if Pointer(Str) = nil then
    begin
      Pointer(Str) := Temp;
      Exit;
    end;
    CopyCount := __StringLength(Str);
    if CopyCount > NewLength then
      CopyCount := NewLength;
    Move(PAnsiChar(str)^, PAnsiChar(Temp)^, CopyCount);
    Pointer(Str) := Temp;
  end;
end;
{$ELSE}

procedure       _LStrSetLength{ var str: AnsiString; newLength: Integer; CodePage: Word };
asm
        { ->    EAX     Pointer to str  }
        {       EDX     new length      }
        {       ECX     codePage        }

{$IFDEF ALIGN_STACK}
        SUB     ESP, 8                       // Stack - xxxxxxx4
{$ENDIF ALIGN_STACK}
        PUSH    EBX                          // Stack - xxxxxxx0
        PUSH    ESI                          // Stack - xxxxxxxc
        PUSH    EDI                          // Stack - xxxxxxx8
        PUSH    EBP                          // Stack - xxxxxxx4
        PUSH    0                            // Stack - xxxxxxx0 - Local Temp
        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EBP,ECX
        XOR     EDI,EDI

        TEST    EDX,EDX
        JLE     @@setString

        MOV     EAX,[EBX]
        TEST    EAX,EAX
        JE      @@copyString

        CMP     [EAX-skew].StrRec.refCnt,1
        JNE     @@copyString

        SUB     EAX,rOff
        ADD     EDX,rOff+1
        JO      @@overflow
        MOV     [ESP],EAX                    // Stack - xxxxxxx0
        MOV     EAX,ESP
        CALL    _ReallocMem
        MOV     EAX,[ESP]                    // Stack - xxxxxxx0
        ADD     EAX,rOff
        MOV     [EBX],EAX
        MOV     [EAX-skew].StrRec.length,ESI
        MOV     BYTE PTR [EAX+ESI],0
        JMP     @@exit

@@overflow:
        JMP     _IntOver

@@copyString:
        MOV     EAX,EDX
        MOV     EDX,EBP
        CALL    _NewAnsiString
        MOV     EDI,EAX

        MOV     EAX,[EBX]
        TEST    EAX,EAX
        JE      @@setString

        MOV     EDX,EDI
        MOV     ECX,[EAX-skew].StrRec.length
        CMP     ECX,ESI
        JL      @@moveString
        MOV     ECX,ESI

@@moveString:
        CALL    Move

@@setString:
        MOV     EAX,EBX
        CALL    _LStrClr
        MOV     [EBX],EDI

@@exit:
        POP     EDX                          // Stack - xxxxxxx4 - Local Temp
        POP     EBP                          // Stack - xxxxxxx8
        POP     EDI                          // Stack - xxxxxxxc
        POP     ESI                          // Stack - xxxxxxx0
        POP     EBX                          // Stack - xxxxxxx4
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8                       // Stack - xxxxxxxc
{$ENDIF ALIGN_STACK}
end;
{$IFEND}




procedure _LStrFromUStr(var Dest: AnsiString; const Source: UnicodeString; CodePage: Word);
{$IFDEF PUREPASCAL}
begin
  if Pointer(Source) = nil then
    _LStrClr(Dest)
  else
    _LStrFromPWCharLen(Dest, PWideChar(Pointer(Source)), __StringLength(Source), CodePage)
end;
{$ELSE}
asm
        { ->    EAX pointer to dest                 }
        {       EDX pointer to UnicodeString data   }
        {       ECX destination codepage            }

{$IFDEF ALIGN_STACK}
        SUB     ESP,8
        PUSH    ECX
{$ELSE !ALIGN_STACK}
        PUSH    [ESP]
        MOV     [ESP+4],ECX
{$ENDIF !ALIGN_STACK}
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@1
        MOV     ECX,[EDX-Skew].StrRec.length     // length in UnicodeString is widechar count
@@1:
{$IFDEF ALIGN_STACK}
        CALL    _LStrFromPWCharLen
        ADD     ESP,8
        RET
{$ELSE !ALIGN_STACK}
        JMP     _LStrFromPWCharLen
{$ENDIF !ALIGN_STACK}
end;
{$ENDIF !PUREPASCAL}


{ ----------------------------------------------------- }
{       Compiler helper for WideString support          }
{ ----------------------------------------------------- }

procedure UStrSet(var S: UnicodeString; P: PWideChar); forward;


procedure WStrSet(var S: WideString; P: PWideChar);
{$IFDEF PUREPASCAL}
var
  Temp: Pointer;
begin
  Temp := Pointer(InterlockedExchange(Pointer(S), Pointer(P)));
  if Temp <> nil then
    _WStrClr(Temp);
end;
{$ELSE}
{$IFDEF POSIX}
asm
        JMP     UStrSet
end;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
asm
        XCHG    [EAX],EDX
        TEST    EDX,EDX
        JZ      @@1
        PUSH    EDX
        CALL    SysFreeString
@@1:
end;
{$ENDIF MSWINDOWS}
{$ENDIF !PUREPASCAL}





procedure _WStrFromChar(var Dest: WideString; Source: AnsiChar);
{$IF not defined(CPU386)}
begin
  InternalWStrFromPCharLen(Dest, @Source, 1, DefaultSystemCodePage);
end;
{$ELSE}
{$IFDEF MSWINDOWS}
asm
        PUSH    EDX
        MOV     EDX,ESP
        MOV     ECX,1
        PUSH    DefaultSystemCodePage
        CALL    InternalWStrFromPCharLen
        POP     EDX
end;
{$ENDIF}
{$IFDEF POSIX}
asm
        JMP     _UStrFromChar
end;
{$ENDIF}
{$IFEND}




procedure _WStrFromWChar(var Dest: WideString; Source: WideChar);
{$IF not defined(CPU386)}
begin
  _WStrFromPWCharLen(Dest, @Source, 1);
end;
{$ELSE}
{$IFDEF MSWINDOWS}
asm
        { ->    EAX     Pointer to WideString (dest)   }
        {       EDX     character             (source) }
        PUSH    EDX
        MOV     EDX,ESP
        MOV     ECX,1
        CALL    _WStrFromPWCharLen
        POP     EDX
end;
{$ENDIF}
{$IFDEF POSIX}
asm
        JMP _UStrFromWChar
end;
{$ENDIF}
{$IFEND}




procedure _WStrFromPChar(var Dest: WideString; Source: PAnsiChar);
{$IF not defined(CPU386)}
begin
  InternalWStrFromPCharLen(Dest, Source, _PCharLen(Source), DefaultSystemCodePage);
end;
{$ELSE}
{$IFDEF MSWINDOWS}
asm
        { ->    EAX     Pointer to WideString (dest)    }
        {       EDX     Pointer to character  (source)  }
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CL,[EDX+0]
        JE      @@4
        CMP     CL,[EDX+1]
        JE      @@3
        CMP     CL,[EDX+2]
        JE      @@2
        CMP     CL,[EDX+3]
        JE      @@1
        ADD     EDX,4
        JMP     @@0
@@1:    INC     EDX
@@2:    INC     EDX
@@3:    INC     EDX
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
@@5:    JMP     _WStrFromPCharLen
end;
{$ENDIF}
{$IFDEF POSIX}
asm
        JMP     _UStrFromPChar
end;
{$ENDIF}
{$IFEND}




procedure _WStrFromPWChar(var Dest: WideString; Source: PWideChar);
{$IF not defined(CPU386)}
begin
  if Pointer(Source) = nil then
    _WStrClr(Dest)
  else
    _WStrFromPWCharLen(Dest, Source, _PWCharLen(Source));
end;
{$ELSE}
{$IFDEF MSWINDOWS}
asm
        { ->    EAX     Pointer to WideString (dest)   }
        {       EDX     Pointer to character  (source) }
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CX,[EDX+0]
        JE      @@4
        CMP     CX,[EDX+2]
        JE      @@3
        CMP     CX,[EDX+4]
        JE      @@2
        CMP     CX,[EDX+6]
        JE      @@1
        ADD     EDX,8
        JMP     @@0
@@1:    ADD     EDX,2
@@2:    ADD     EDX,2
@@3:    ADD     EDX,2
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
        SHR     ECX,1
@@5:    JMP     _WStrFromPWCharLen
end;
{$ENDIF}
{$IFDEF POSIX}
asm
        JMP     _UStrFromPWChar
end;
{$ENDIF}
{$IFEND}




procedure _WStrFromString(var Dest: WideString; const Source: ShortString);
{$IF not defined(CPU386)}
begin
  InternalWStrFromPCharLen(Dest, @Source[1], Byte(Source[0]), DefaultSystemCodePage);
end;
{$ELSE}
{$IFDEF MSWINDOWS}
asm
        XOR     ECX,ECX
        MOV     CL,[EDX]
        INC     EDX
        JMP     _WStrFromPCharLen
end;
{$ENDIF}
{$IFDEF POSIX}
asm
        JMP    _UStrFromString
end;
{$ENDIF}
{$IFEND}




procedure _WStrFromArray(var Dest: WideString; Source: PAnsiChar; Length: Integer);
{$IF not defined(CPU386)}
var
  P: PAnsiChar;
begin
  P := Source;
  while (Length > 0) and (P^ <> #0) do
  begin
    Dec(Length);
    Inc(P);
  end;
  Length := P - Source;
  InternalWStrFromPCharLen(Dest, Source, Length, DefaultSystemCodePage);
end;
{$ELSE}
{$IFDEF MSWINDOWS}
asm
        { ->    EAX     Pointer to WideString (dest)   }
        {       EDX     Pointer to character  (source) }
        {       ECX     Length of source characters    }
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASB
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        JMP     _WStrFromPCharLen
end;
{$ENDIF}
{$IFDEF POSIX}
asm
        JMP     _UStrFromArray
end;
{$ENDIF}
{$IFEND}




procedure _WStrFromWArray(var Dest: WideString; Source: PWideChar; Length: Integer);
{$IF not defined(CPU386)}
var
  P: PWideChar;
begin
  P := Source;
  while (Length > 0) and (P^ <> #0) do
  begin
    Dec(Length);
    Inc(P);
  end;
  Length := P - Source;
  _WStrFromPWCharLen(Dest, Source, Length);
end;
{$ELSE}
{$IFDEF MSWINDOWS}
asm
        { ->    EAX     Pointer to WideString (dest)   }
        {       EDX     Pointer to character  (source) }
        {       ECX     Length of source characters    }
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASW
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        JMP     _WStrFromPWCharLen
end;
{$ENDIF}
{$IFDEF POSIX}
asm
        JMP     _UStrFromWArray
end;
{$ENDIF}
{$IFEND}




procedure _WStrFromLStr(var Dest: WideString; const Source: AnsiString);
{$IF not defined(CPU386)}
var
  Len: Integer;
begin
  if Pointer(Source) = nil then
    _WStrClr(Dest)
  else
  begin
    Len := __StringLength(Source);
    InternalWStrFromPCharLen(Dest, PAnsiChar(Pointer(Source)), Len, __StringCodePage(Source))
  end;
end;
{$ELSE}
{$IFDEF MSWINDOWS}
asm
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@1
// Inject the CodePage parameter onto the stack ahead of the return address
        MOVZX   ECX,[EDX-Skew].StrRec.codePage
        PUSH    [ESP]
        MOV     [ESP+4],ECX

        MOV     ECX,[EDX-Skew].StrRec.length
        JMP     InternalWStrFromPCharLen
@@1:    JMP     _WStrClr
end;
{$ENDIF}
{$IFDEF POSIX}
asm
        JMP     _UStrFromLStr
end;
{$ENDIF}
{$IFEND}


procedure _WStrToString(Dest: PShortString; const Source: WideString; MaxLen: Integer);
var
  SourceLen, DestLen: Integer;
  Buffer: array[0..511] of AnsiChar;
begin
  if MaxLen > 255 then MaxLen := 255;
  SourceLen := Length(Source);
  if SourceLen >= MaxLen then SourceLen := MaxLen;
  if SourceLen = 0 then
    DestLen := 0
  else
  begin
    DestLen := CharFromWChar(Buffer, High(Buffer), PWideChar(Pointer(Source)), SourceLen);
    if DestLen < 0 then
      DestLen := 0
    else if DestLen > MaxLen then
      DestLen := MaxLen;
  end;
  Byte(Dest^[0]) := DestLen;
  if DestLen > 0 then Move(Buffer, Dest^[1], DestLen);
end;





function _WStrToPWChar(const S: WideString): PWideChar;
const
  EmptyString = '';
begin
  if Pointer(S) = nil then
    Result := EmptyString
  else
    Result := Pointer(S);
end;

procedure _WStrCat(var Dest: WideString; const Source: WideString);
var
  DestLen, SourceLen: Integer;
  NewStr: PWideChar;
begin
  SourceLen := Length(Source);
  if SourceLen <> 0 then
  begin
    DestLen := Length(Dest);
    NewStr := _NewWideString(DestLen + SourceLen);
    if DestLen > 0 then
      Move(Pointer(Dest)^, NewStr^, DestLen * sizeof(WideChar));
    Move(Pointer(Source)^, NewStr[DestLen], SourceLen * sizeof(WideChar));
    WStrSet(Dest, NewStr);
  end;
end;

procedure _WStrCat3(var Dest: WideString; const Source1, Source2: WideString);
var
  Source1Len, Source2Len: Integer;
  NewStr: PWideChar;
begin
  Source1Len := Length(Source1);
  Source2Len := Length(Source2);
  if (Source1Len <> 0) or (Source2Len <> 0) then
  begin
    NewStr := _NewWideString(Source1Len + Source2Len);
    Move(Pointer(Source1)^, Pointer(NewStr)^, Source1Len * sizeof(WideChar));
    Move(Pointer(Source2)^, NewStr[Source1Len], Source2Len * sizeof(WideChar));
    WStrSet(Dest, NewStr);
  end
  else
    _WStrClr(Dest);
end;



procedure _WStrCatN{var Dest: WideString; ArgCnt: Integer; ...};
{$IF not defined(CPU386)}
begin
end;
{$ELSE}
{$IFDEF MSWINDOWS}
asm
        {     ->EAX = Pointer to dest }
        {       EDX = number of args (>= 3) }
        {       [ESP+4], [ESP+8], ... crgCnt WideString arguments }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDX
        PUSH    EAX
        MOV     EBX,EDX

        XOR     EAX,EAX
@@loop1:
        MOV     ECX,[ESP+EDX*4+4*4]
        TEST    ECX,ECX
        JE      @@1
        ADD     EAX,[ECX-4]
@@1:
        DEC     EDX
        JNE     @@loop1

        SHR     EAX,1
        CALL    _NewWideString
        PUSH    EAX
        MOV     ESI,EAX

@@loop2:
        MOV     EAX,[ESP+EBX*4+5*4]
        MOV     EDX,ESI
        TEST    EAX,EAX
        JE      @@2
        MOV     ECX,[EAX-4]
        ADD     ESI,ECX
        CALL    Move
@@2:
        DEC     EBX
        JNE     @@loop2

        POP     EDX
        POP     EAX
        CALL    WStrSet

        POP     EDX
        POP     ESI
        POP     EBX
        POP     EAX
        LEA     ESP,[ESP+EDX*4]
        JMP     EAX
end;
{$ENDIF MSWINDOWS}


{$IFDEF POSIX}
asm
        {     ->EAX = Pointer to dest }
        {       EDX = number of args (>= 3) }
        {       [ESP+4], [ESP+8], ... crgCnt WideString arguments }

        JMP    _UStrCatN
end;
{$ENDIF POSIX}
{$IFEND}

{$IF not defined(CPU386)}
// Returns 0 : Left = Right
//     minus : Left < Right
//      plus : Left > Right
function _WStrCmp(const Left, Right: WideString): Integer;
var
  Len, LLen, RLen: Integer;
  LPtr, RPtr: PWideChar;
begin
  if Pointer(Left) = Pointer(Right) then
    Result := 0
  else if Pointer(Left) = nil then
    Result := 0 - __StringLength(Right)
  else if Pointer(Right) = nil then
    Result := __StringLength(Left)
  else
  begin
    LLen := __StringLength(Left);
    RLen := __StringLength(Right);
    Len := LLen;
    if Len > RLen then Len := RLen;
    LPtr := PWideChar(Left);
    RPtr := PWideChar(Right);
    while Len > 0 do
    begin
      Result := Ord(LPtr^) - Ord(RPtr^);
      if Result <> 0 then
        Exit;
      if Len = 1 then break;
      Result := Ord(LPtr[1]) - Ord(RPtr[1]);
      if Result <> 0 then
        Exit;
      Inc(LPtr, 2);
      Inc(RPtr, 2);
      Dec(Len, 2);
    end;
    Result := LLen - RLen;
  end;
end;
{$ELSE}
procedure _WStrCmp{Left, Right: WideString};
{$IFDEF MSWINDOWS}


asm //StackAlignSafe
        {     ->EAX = Pointer to left string    }
        {       EDX = Pointer to right string   }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX

        CMP     EAX,EDX
        JE      @@exit

        TEST    ESI,ESI
        JE      @@str1null

        TEST    EDI,EDI
        JE      @@str2null

        MOV     EAX,[ESI-4]
        MOV     EDX,[EDI-4]

        SUB     EAX,EDX { eax = len1 - len2 }
        JA      @@skip1 { len1 > len2 (unsigned)? }
        ADD     EDX,EAX { edx = len2 + (len1 - len2) = len1     }
                        // edx := Min(len1, len2)
@@skip1:
        PUSH    EDX
        SHR     EDX,2
        JE      @@cmpRest
@@longLoop:
        MOV     ECX,[ESI]
        MOV     EBX,[EDI]
        CMP     ECX,EBX
        JNE     @@misMatch
        DEC     EDX
        JE      @@cmpRestP4
        MOV     ECX,[ESI+4]
        MOV     EBX,[EDI+4]
        CMP     ECX,EBX
        JNE     @@misMatch
        ADD     ESI,8
        ADD     EDI,8
        DEC     EDX
        JNE     @@longLoop
        JMP     @@cmpRest
@@cmpRestP4:
        ADD     ESI,4
        ADD     EDI,4
@@cmpRest:
        POP     EDX
        AND     EDX,2
        JE      @@equal

        MOV     CX,[ESI]
        MOV     BX,[EDI]
        CMP     CX,BX
        JNE     @@exit

@@equal:
        ADD     EAX,EAX
        JMP     @@exit

@@str1null:
        MOV     EDX,[EDI-4]
        SUB     EAX,EDX
        JMP     @@exit

@@str2null:
        MOV     EAX,[ESI-4]
        SUB     EAX,EDX
        JMP     @@exit

@@misMatch:
        POP     EDX
        CMP     CX,BX
        JNE     @@exit
        SHR     ECX,16
        SHR     EBX,16
        CMP     CX,BX

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF MSWINDOWS}


{$IFDEF POSIX}
asm
        {     ->EAX = Pointer to left string    }
        {       EDX = Pointer to right string   }

        JMP     _UStrCmp
end;
{$ENDIF POSIX}
{$IFEND}



{$IF not defined(CPU386)}
function _WStrEqual(const Left, Right: WideString): Integer;
begin
  Result := _WStrCmp(Left, Right);
end;
{$ELSE}
procedure _WStrEqual{const Left, Right: WideString};
{$IFDEF MSWINDOWS}
asm
        JMP     _WStrCmp
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
asm
        JMP     _UStrCmp
end;
{$ENDIF POSIX}
{$IFEND}

function _WStrCopy(const S: WideString; Index, Count: Integer): WideString;
var
  L, N: Integer;
begin
  L := Length(S);
  if Index < 1 then Index := 0 else
  begin
    Dec(Index);
    if Index > L then Index := L;
  end;
  if Count < 0 then N := 0 else
  begin
    N := L - Index;
    if N > Count then N := Count;
  end;
  _WStrFromPWCharLen(Result, PWideChar(Pointer(S)) + Index, N);
end;

procedure _WStrDelete(var S: WideString; Index, Count: Integer);
var
  L, N: Integer;
  NewStr: PWideChar;
begin
  L := Length(S);
  if (L > 0) and (Index >= 1) and (Index <= L) and (Count > 0) then
  begin
    Dec(Index);
    N := L - Index - Count;
    if N < 0 then N := 0;
    if (Index = 0) and (N = 0) then NewStr := nil else
    begin
      NewStr := _NewWideString(Index + N);
      if Index > 0 then
        Move(Pointer(S)^, NewStr^, Index * 2);
      if N > 0 then
        Move(PWideChar(Pointer(S))[L - N], NewStr[Index], N * 2);
    end;
    WStrSet(S, NewStr);
  end;
end;

procedure _WStrInsert(const Source: WideString; var Dest: WideString; Index: Integer);
var
  SourceLen, DestLen: Integer;
  NewStr: PWideChar;
begin
  SourceLen := Length(Source);
  if SourceLen > 0 then
  begin
    DestLen := Length(Dest);
    if Index < 1 then Index := 0 else
    begin
      Dec(Index);
      if Index > DestLen then Index := DestLen;
    end;
    NewStr := _NewWideString(DestLen + SourceLen);
    if Index > 0 then
      Move(Pointer(Dest)^, NewStr^, Index * 2);
    Move(Pointer(Source)^, NewStr[Index], SourceLen * 2);
    if Index < DestLen then
      Move(PWideChar(Pointer(Dest))[Index], NewStr[Index + SourceLen],
        (DestLen - Index) * 2);
    WStrSet(Dest, NewStr);
  end;
end;

procedure _WStrSetLength(var S: WideString; NewLength: Integer);
var
  NewStr: PWideChar;
  Count: Integer;
begin
  NewStr := nil;
  if NewLength > 0 then
  begin
    NewStr := _NewWideString(NewLength);
    Count := Length(S);
    if Count > 0 then
    begin
      if Count > NewLength then Count := NewLength;
      Move(Pointer(S)^, NewStr^, Count * SizeOf(WideChar));
    end;
  end;
  WStrSet(S, NewStr);
end;

procedure _WCharToString(Dest: PShortString; const Source: WideChar; MaxLen: Integer);
var
  DestLen: Integer;
  Buffer: array[0..255] of AnsiChar;
begin
  if MaxLen > 255 then MaxLen := 255;
  DestLen := CharFromWChar(Buffer, High(Buffer), @Source, 1);
  if DestLen < 0 then
    DestLen := 0
  else if DestLen > MaxLen then
    DestLen := MaxLen;
  Byte(Dest^[0]) := DestLen;
  if DestLen > 0 then Move(Buffer, Dest^[1], DestLen);
end;


{ ----------------------------------------------------- }
{       Compiler helper for UnicodeString support       }
{ ----------------------------------------------------- }

{ UnicodeString helper functions }

const
  // Using initialized constant to be sure of alignment.
  // Not as read-only as code segment, but code entry points
  // have no alignment guarantees.
  EmptyStringW: TEmptyString = (
    Rec: (
      codePage: Word($FFFF);
      elemSize: 2;
      refCnt: -1;
      length: 0);
    Nul: 0);





function _UStrToPWChar(const S: UnicodeString): PWideChar;
begin
  if Pointer(S) = nil then
    Result := @EmptyStringW.Nul
  else
    Result := Pointer(S);
end;



procedure _UStrFromChar(var Dest: UnicodeString; Source: AnsiChar);
{$IF not defined(CPU386)}
begin
  _UStrFromPCharLen(Dest, @Source, 1);
end;
{$ELSE}
asm //StackAlignSafe
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EDX      // char on stack
        MOV     EDX,ESP  // addr of char on stack in EDX
        MOV     ECX,1
        CALL    _UStrFromPCharLen
        POP     EDX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
end;
{$IFEND}


procedure _UStrFromWChar(var Dest: UnicodeString; Source: WideChar);
{$IF not defined(CPU386)}

begin
  _UStrFromPWCharLen(Dest, @Source, 1);
end;
{$ELSE}
asm //StackAlignSafe
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EDX
        MOV     EDX,ESP
        MOV     ECX,1
        CALL    _UStrFromPWCharLen
        POP     EDX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
end;
{$IFEND}




procedure _UStrFromPChar(var Dest: UnicodeString; Source: PAnsiChar);
{$IF not defined(CPU386)}
begin
  _UStrFromPCharLen(Dest, Source, _PCharLen(Source));
end;
{$ELSE}
asm
        { ->    EAX     Pointer to WideString (dest)   }
        {       EDX     Pointer to character  (source) }
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CL,[EDX+0]
        JE      @@4
        CMP     CL,[EDX+1]
        JE      @@3
        CMP     CL,[EDX+2]
        JE      @@2
        CMP     CL,[EDX+3]
        JE      @@1
        ADD     EDX,4
        JMP     @@0
@@1:    INC     EDX
@@2:    INC     EDX
@@3:    INC     EDX
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
@@5:    JMP     _UStrFromPCharLen
end;
{$IFEND}




procedure _UStrFromPWChar(var Dest: UnicodeString; Source: PWideChar);
{$IF not defined(CPU386)}
begin
  _UStrFromPWCharLen(Dest, Source, _PWCharLen(Source));
end;
{$ELSE}
asm
        { ->    EAX     Pointer to WideString (dest)   }
        {       EDX     Pointer to character  (source) }
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@5
        PUSH    EDX
@@0:    CMP     CX,[EDX+0]
        JE      @@4
        CMP     CX,[EDX+2]
        JE      @@3
        CMP     CX,[EDX+4]
        JE      @@2
        CMP     CX,[EDX+6]
        JE      @@1
        ADD     EDX,8
        JMP     @@0
@@1:    ADD     EDX,2
@@2:    ADD     EDX,2
@@3:    ADD     EDX,2
@@4:    MOV     ECX,EDX
        POP     EDX
        SUB     ECX,EDX
        SHR     ECX,1
@@5:    JMP     _UStrFromPWCharLen
end;
{$IFEND}




procedure _UStrFromArray(var Dest: UnicodeString; Source: PAnsiChar; Length: Integer);
{$IF not defined(CPU386)}
var
  P: PAnsiChar;
begin
  P := Source;
  while (Length > 0) and (P^ <> #0) do
  begin
    Dec(Length);
    Inc(P);
  end;
  Length := P - Source;
  _UStrFromPCharLen(Dest, Source, Length);
end;
{$ELSE}
asm
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASB      // find #0
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        JMP     _UStrFromPCharLen
end;
{$IFEND}




procedure _UStrFromWArray(var Dest: UnicodeString; Source: PWideChar; Length: Integer);
{$IF not defined(CPU386)}
var
  P: PWideChar;
begin
  P := Source;
  while (Length > 0) and (P^ <> #0) do
  begin
    Dec(Length);
    Inc(P);
  end;
  Length := P - Source;
  _UStrFromPWCharLen(Dest, Source, Length);
end;
{$ELSE}
asm
        PUSH    EDI
        PUSH    EAX
        PUSH    ECX
        MOV     EDI,EDX
        XOR     EAX,EAX
        REPNE   SCASW     // find #$0000
        JNE     @@1
        NOT     ECX
@@1:    POP     EAX
        ADD     ECX,EAX
        POP     EAX
        POP     EDI
        JMP     _UStrFromPWCharLen
end;
{$IFEND}



procedure _UStrFromLStr(var Dest: UnicodeString; const Source: AnsiString);
{$IF not defined(CPU386)}
begin
  if Pointer(Source) = nil then
    _UStrClr(Dest)
  else
    InternalUStrFromPCharLen(Dest, PAnsiChar(Pointer(Source)), __StringLength(Source), __StringCodePage(Source));
end;
{$ELSE}
asm //StackAlignSafe
        XOR     ECX,ECX
        TEST    EDX,EDX
        JE      @@1
        MOVZX   ECX,WORD PTR [EDX-Skew].StrRec.codePage
{$IFDEF ALIGN_STACK}
        PUSH    EBP         // Need a stack frame setup for the
        MOV     EBP, ESP    // unwinder in case there's an Exception
        SUB     ESP, 4
        PUSH    ECX
        MOV     ECX,[EDX-Skew].StrRec.length
        CALL    InternalUStrFromPCharLen
        ADD     ESP, 4
        POP     EBP
        RET
{$ELSE}
        PUSH    [ESP]
        MOV     [ESP+4],ECX
        MOV     ECX,[EDX-Skew].StrRec.length
        JMP     InternalUStrFromPCharLen
{$ENDIF ALIGN_STACK}
@@1:    JMP     _UStrFromPCharLen
end;
{$IFEND}



procedure _UStrFromWStr(var Dest: UnicodeString; const Source: WideString);
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
  _UStrAsg(Dest, UnicodeString(Pointer(Source)));
end;
{$ELSE}
asm
        JMP     _UStrAsg
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
{$IFDEF PUREPASCAL}
var
  Len: Integer;
begin
  Len := 0;
  if Pointer(Source) <> nil then
    Len := __StringLength(Source);
  _UStrFromPWCharLen(Dest, PWideChar(Pointer(Source)), Len);
end;
{$ELSE}
asm
        { ->    EAX pointer to dest                 }
        {       EDX pointer to WideString data      }

        XOR     ECX,ECX
        TEST    EDX,EDX
        JZ      @@1            // nil source => zero length
        MOV     ECX,[EDX-4]
        SHR     ECX,1          // length in WideString is byte count
@@1:    JMP     _UStrFromPWCharLen
end;
{$ENDIF !PUREPASCAL}
{$ENDIF MSWINDOWS}



procedure _WStrFromUStr(var Dest: WideString; const Source: UnicodeString);
{$IFDEF POSIX}
{$IFDEF PUREPASCAL}
begin
  _WStrAsg(Dest, WideString(Pointer(Source)));
end;
{$ELSE}
asm
        JMP     _WStrAsg
end;
{$ENDIF !PUREPASCAL}
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
{$IF not defined(CPU386)}
var
  Len: Integer;
begin
  if Pointer(Source) = nil then
    _WStrClr(Dest)
  else
    _WStrFromPWCharLen(Dest, PWideChar(Pointer(Source)), __StringLength(Source))
end;
{$ELSE}
asm
        { ->    EAX pointer to dest                 }
        {       EDX pointer to UnicodeString data   }

        XOR     ECX,ECX
        TEST    EDX,EDX
        JZ      @@1            // nil source => zero length
        MOV     ECX,[EDX-Skew].StrRec.length    // length in UnicodeString is widechar count
@@1:    JMP     _WStrFromPWCharLen
end;
{$IFEND}
{$ENDIF MSWINDOWS}

procedure _UStrToString(Dest: PShortString; const Source: UnicodeString; MaxLen: Integer);
var
  SourceLen, DestLen: Integer;
  Buffer: array[0..511] of AnsiChar;
begin
  if MaxLen > 255 then MaxLen := 255;
  SourceLen := Length(Source);
  if SourceLen >= MaxLen then SourceLen := MaxLen;
  if SourceLen = 0 then
    DestLen := 0
  else
  begin
    DestLen := CharFromWChar(Buffer, High(Buffer), PWideChar(Pointer(Source)), SourceLen);
    if DestLen < 0 then
      DestLen := 0
    else if DestLen > MaxLen then
      DestLen := MaxLen;
  end;
  Byte(Dest^[0]) := DestLen;
  if DestLen > 0 then Move(Buffer, Dest^[1], DestLen);
end;




procedure _UStrFromString(var Dest: UnicodeString; const Source: ShortString);
{$IF not defined(CPU386)}
begin
  InternalUStrFromPCharLen(Dest, @Source[1], Byte(Source[0]), DefaultSystemCodePage);
end;
{$ELSE}
asm
        XOR     ECX,ECX
        MOV     CL,[EDX]
        INC     EDX
        JMP     _UStrFromPCharLen
end;
{$IFEND}



procedure _UStrSetLength(var Str: UnicodeString; NewLength: Integer);
{$IF not defined(CPU386)}
var
  P: PStrRec;
  Temp: Pointer;
  CopyCount: Integer;
begin
  if NewLength <= 0 then
    _UStrClr(Str)
  else
  begin
    if Pointer(Str) <> nil then
    begin
      if __StringRefCnt(Str) = 1 then
      begin
        P := Pointer(NativeInt(Str) - Sizeof(StrRec));
        if Cardinal(NewLength) >= Cardinal(- SizeOf(StrRec) - SizeOf(WideChar)) div 2 then
          _IntOver;
        _ReallocMem(Pointer(P), (NewLength + 1) * SizeOf(WideChar) + SizeOf(StrRec));
        P.length := NewLength;
        Pointer(Str) := Pointer(NativeInt(P) + SizeOf(StrRec));
        PWideChar(Str)[NewLength] := #0;
        Exit;
      end;
    end;
    Temp := _NewUnicodeString(NewLength);
    if Pointer(Str) = nil then
    begin
      Pointer(Str) := Temp;
      Exit;
    end;
    CopyCount := __StringLength(Str);
    if CopyCount > NewLength then
      CopyCount := NewLength;
    Move(PWideChar(Str)^, PWideChar(Temp)^, CopyCount * SizeOf(WideChar));
  end;
end;
{$ELSE}
asm //StackAlignSafe
        { ->    EAX     Pointer to S  }
        {       EDX     new length    }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX       // EBX saves @S
        MOV     ESI,EDX       // ESI saves NewLength (chars)
        XOR     EDI,EDI       // EDI := 0; EDI is Temp (result)

        TEST    EDX,EDX       // NewLength <= 0?
        JLE     @@setString   // Assign S := Temp

        MOV     EAX,[EBX]     // EAX := S
        TEST    EAX,EAX       // nil?
        JE      @@copyString  // cannot reallocate (it's nil), so copy

        CMP     [EAX-skew].StrRec.refCnt,1 // !!! MT safety
        JNE     @@copyString  // not unique, so copy

        SUB     EAX,rOff      // Offset EAX "S" to start of memory block
        ADD     EDX,EDX       // Double length to get size
        JO      @@overflow
        ADD     EDX,rOff+2    // Add string rec size
        JO      @@overflow
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EAX           // Put S on stack
        MOV     EAX,ESP       // to pass by reference
        CALL    _ReallocMem
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        ADD     EAX,rOff      // Readjust
        MOV     [EBX],EAX     // Store
        MOV     [EAX-skew].StrRec.length,ESI
        MOV     WORD PTR [EAX+ESI*2],0 // Null terminate
        TEST    EDI,EDI       // Was a temp created?
        JZ      @@exit
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EDI
        MOV     EAX,ESP
        CALL    _LStrClr
        POP     EDI
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        JMP     @@exit

@@overflow:
        JMP     _IntOver

@@copyString:
        MOV     EAX,EDX       // EAX := NewLength
        CALL    _NewUnicodeString
        MOV     EDI,EAX       // EDI "Temp" := new string

        MOV     EAX,[EBX]     // EAX := S, also Source of Move
        TEST    EAX,EAX       // nil?
        JE      @@setString   // Assign straight away

        MOV     EDX,EDI       // EDX := EDI "Temp", also Dest of Move
        MOV     ECX,[EAX-skew].StrRec.length  // ECX := Length(S), also Count of Move
        CMP     ECX,ESI       // ECX "Length(S)" <> NewLength
        JL      @@moveString  // ECX smaller => jump
        MOV     ECX,ESI       // ECX := ESI

@@moveString:
        SHL     ECX,1         // Length widechars to bytes translation
        CALL    Move          // Move ECX chars from EAX to EDX

@@setString:
        MOV     EAX,EBX       // EAX := @S
        CALL    _LStrClr      // clear S
        MOV     [EBX],EDI     // S := Temp

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$IFEND}



procedure _UStrCat(var Dest: UnicodeString; const Source: UnicodeString);
{$IF not defined(CPU386)}
var
  L1, L2, Len: Cardinal;
  Src: Pointer;
begin
  if Pointer(Source) <> nil then
  begin
    if Pointer(Dest) = nil then
      _UStrAsg(Dest, Source)
    else
    begin
      Src := Pointer(Source);
      if Pointer(Dest) = Pointer(Source) then
      begin
        L1 := __StringLength(Dest);
        L2 := L1;
      end
      else
      begin
        L1 := __StringLength(Dest);
        L2 := __StringLength(Src);
      end;
      Len := L1 + L2;
      if (((L1 and L2) or ((not Len) and (L1 or L2))) and $80000000) <> 0 then _IntOver;
      _UStrSetLength(Dest, Len);
      Move(PWideChar(Src)^, PWideChar(Dest)[L1], L2 * SizeOf(WideChar));
    end;
  end;
end;
{$ELSE}
asm //StackAlignSafe
        { ->    EAX     pointer to dest }
        {       EDX     source          }

        TEST    EDX,EDX       // Source empty, nop.
        JE      @@exit

        MOV     ECX,[EAX]     // ECX := Dest
        TEST    ECX,ECX       // Nil source => assignment
        JE      _UStrAsg

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX         // EBX := @Dest
        MOV     ESI,EDX         // ESI := Source
        MOV     EDI,[ECX-skew].StrRec.length  // EDI := Length(Dest)

        MOV     EDX,[ESI-skew].StrRec.length  // EDX := Length(Source)
        ADD     EDX,EDI         // EDX := (Length(Source) + Length(Dest)) * 2
        TEST    EDX,$C0000000
        JNZ     @@lengthOverflow
        CMP     ESI,ECX
        JE      @@appendSelf

        CALL    _UStrSetLength  // Set length of Dest
        MOV     EAX,ESI         // EAX := Source
        MOV     ECX,[ESI-skew].StrRec.length // ECX := Length(Source)

@@appendStr:
        MOV     EDX,[EBX]       // EDX := Dest
        SHL     EDI,1           // EDI to bytes (Length(Dest) * 2)
        ADD     EDX,EDI         // Offset EDX for destination of move
        SHL     ECX,1           // convert Length(Source) to bytes
        CALL    Move            // Move(Source, Dest + Length(Dest)*2, Length(Source)*2)
        POP     EDI
        POP     ESI
        POP     EBX
        RET

@@appendSelf:
        CALL    _UStrSetLength
        MOV     EAX,[EBX]
        MOV     ECX,EDI
        JMP     @@appendStr

@@lengthOverflow:
        JMP     _IntOver

@@exit:
end;
{$IFEND}



procedure _UStrCat3(var Dest: UnicodeString; const Source1, Source2: UnicodeString);
{$IF not defined(CPU386)}
var
  CodePage: Word;
  S1, S2: Pointer;
  Temp: Pointer;
  L1, L2, Len: Cardinal;
begin
  if Pointer(Source1) = nil then
    _UStrAsg(Dest, Source2)
  else if Pointer(Source2) = nil then
    _UStrAsg(Dest, Source1)
  else
  begin
    if Pointer(Dest) = Pointer(Source1) then
      _UStrCat(Dest, Source2)
    else if Pointer(Dest) = Pointer(Source2) then
    begin
      L1 := __StringLength(Source1);
      L2 := __StringLength(Source2);
      Len := L1 + L2;
      if (((L1 and L2) or ((not Len) and (L1 or L2))) and $80000000) <> 0 then _IntOver;
      Temp := _NewUnicodeString(Len);
      Move(PWideChar(Source1)^, PWideChar(Temp)[0], L1 * SizeOf(WideChar));
      Move(PWideChar(Source2)^, PWideChar(Temp)[L1], L2 * SizeOf(WideChar));
      _UStrClr(Dest);
      Pointer(Dest) := Temp;
    end
    else
    begin
      _UStrAsg(Dest, Source1);
      _UStrCat(Dest, Source2);
    end;
  end;
end;
{$ELSE}
asm //StackAlignSafe
        {     ->EAX = Pointer to dest   }
        {       EDX = source1           }
        {       ECX = source2           }

        TEST    EDX,EDX
        JE      @@assignSource2

        TEST    ECX,ECX
        JE      _UStrAsg

        CMP     EDX,[EAX]
        JE      @@appendToDest

        CMP     ECX,[EAX]
        JE      @@theHardWay

{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    ECX
        CALL    _UStrAsg

        POP     EDX
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        JMP     _UStrCat

@@theHardWay: // s(*EAX,ECX) := source1(EDX) + s(ECX)

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

{$IFDEF ALIGN_STACK}
        SUB     ESP,12
{$ENDIF ALIGN_STACK}
        MOV     EBX,EDX         // EBX := source1
        MOV     ESI,ECX         // ESI := source2
        PUSH    EAX             // Push(@s)

        MOV     EAX,[EBX-skew].StrRec.length
        ADD     EAX,[ESI-skew].StrRec.length

        TEST    EAX,$C0000000   // either of top two bits set => overflow for size
        JNZ     @@overflow
        CALL    _NewUnicodeString   // EAX := new string ("result")

        MOV     EDI,EAX         // EDI := result

        MOV     EDX,EDI         // EDX := result
        MOV     EAX,EBX         // EAX := source1
        MOV     ECX,[EBX-skew].StrRec.length // ECX := Length(source1)
        SHL     ECX,1           // double ECX for bytes
        CALL    Move            // Move(source1, result, Length(source1)*2)

        MOV     EAX,ESI         // EAX := source2
        MOV     ECX,[ESI-skew].StrRec.length // ECX := Length(source2)
        SHL     ECX,1           // ECX => to bytes
        MOV     EDX,[EBX-skew].StrRec.length // EDX := Length(source1)
        SHL     EDX,1           // EDX => to bytes
        ADD     EDX,EDI         // EDX := result + (num bytes in source1)
        CALL    Move            // Move(source2, result+offset, Length(source2)*2)

        POP     EAX             // EAX := Pop() // @s
{$IFDEF ALIGN_STACK}
        ADD     ESP,12
{$ENDIF ALIGN_STACK}
        MOV     EDX,EDI         // EDX := result
        TEST    EDI,EDI
        JE      @@skip          // result is nil? => don't decrement
        DEC     [EDI-skew].StrRec.refCnt    // EDI = local temp str; _UStrAsg will addref, so ensure final refCnt = 1
@@skip:
        CALL    _UStrAsg

        POP     EDI
        POP     ESI
        POP     EBX

        JMP     @@exit

@@assignSource2:
        MOV     EDX,ECX
        JMP     _UStrAsg

@@appendToDest:
        MOV     EDX,ECX
        JMP     _UStrCat

@@overflow:
        JMP     _IntOver

@@exit:
end;
{$IFEND}





procedure _UStrCatN{var dest:UnicodeString; argCnt: Integer; ...};
{$IF not defined(CPU386)}
begin
end;
{$ELSE}
asm //StackAlignSafe
        {     ->EAX = Pointer to dest           }
        {       EDX = number of args (>= 3)     }
        {       [ESP+4], [ESP+8], ... argCnt UnicodeString arguments, reverse order }

        SUB     ESP,4                       // Stack - xxxxxxx8
        PUSH    EBX                         // Stack - xxxxxxx4
        PUSH    ESI                         // Stack - xxxxxxx0
        PUSH    EDI                         // Stack - xxxxxxxc
        PUSH    EDX                         // Stack - xxxxxxx8
        PUSH    EAX                         // Stack - xxxxxxx4
        PUSH    0                           // Stack - xxxxxxx0 - Local Temp
        MOV     EBX,EDX

        XOR     EDI,EDI
        MOV     ECX,[ESP+EDX*4+7*4] // first arg is furthest out
        TEST    ECX,ECX
        JZ      @@0
        CMP     [EAX],ECX          // is dest = first arg?
        JNE     @@0
        MOV     EDI,ECX            // EDI nonzero -> potential appendstr case
        MOV     EAX,[ECX-skew].StrRec.length  // EAX accumulates final length during @@loop1
        DEC     EDX
        JMP     @@loop1
@@0:
        XOR     EAX,EAX
@@loop1:
        MOV     ECX,[ESP+EDX*4+7*4]
        TEST    ECX,ECX
        JE      @@1
        ADD     EAX,[ECX-skew].StrRec.length
        TEST    EAX,$C0000000
        JNZ     @@overflow
        CMP     EDI,ECX          // is dest an arg besides arg1?
        JNE     @@1
        XOR     EDI,EDI          // can't appendstr - dest is multiple args
@@1:
        DEC     EDX
        JNE     @@loop1

@@append:
        TEST    EDI,EDI          // dest is 1st and only 1st arg?
        JZ      @@copy
        MOV     EDX,EAX          // length into EDX
        MOV     EAX,[ESP + 4]    // ptr to str into EAX
        MOV     ESI,[EDI-skew].StrRec.Length  // save old size before realloc
        CALL    _UStrSetLength
        MOV     EDI,[ESP + 4]    // append other strs to dest
        MOV     EAX,[EDI]
        MOV     [ESP],EAX
        SHL     ESI,1            // Length to bytes for offset into string
        ADD     ESI,[EDI]        // ESI = end of old string
        DEC     EBX
        JMP     @@loop2

@@copy:
        CALL    _NewUnicodeString
        MOV     [ESP],EAX
        MOV     ESI,EAX

@@loop2:
        // Loop invariants:
        // - ESI is target of move, going through final dest
        // - EBX is arg index in stack to get arguments;
        //   last argument pushed last => lowest address => addresses decrease from first to last
        MOV     EAX,[ESP+EBX*4+7*4]     // EAX := argN
        MOV     EDX,ESI                 // EDX := dest
        TEST    EAX,EAX                 // argN nil?
        JE      @@2                     // => skip
        MOV     ECX,[EAX-skew].StrRec.length    // ECX := Length(argN)
        SHL     ECX,1                   // ECX to bytes
        ADD     ESI,ECX                 // ESI (running target of move) += ECX
        CALL    Move                    // Move(argN, dest, Length(argN) * 2)
@@2:
        DEC     EBX
        JNE     @@loop2

        MOV     EDX,[ESP]
        MOV     EAX,[ESP + 4]
        TEST    EDI,EDI
        JNZ     @@exit

        TEST    EDX,EDX
        JE      @@skip
        DEC     [EDX-skew].StrRec.refCnt   // EDX = local temp str
@@skip:
        CALL    _UStrAsg

@@exit:
        ADD     ESP,8                     // Stack - xxxxxxx8 - Clean Local Temp & Saved EAX
        POP     EDX                       // Stack - xxxxxxxc
        POP     EDI                       // Stack - xxxxxxx0
        POP     ESI                       // Stack - xxxxxxx4
        POP     EBX                       // Stack - xxxxxxx8
        POP     EAX                       // Stack - xxxxxxxc
        POP     EAX // ret address from CALL Stack - xxxxxxx0
        LEA     ESP,[ESP+EDX*4]
        JMP     EAX // Unbalanced CALL/RET means clobbered branch prediction.
                    // Should fix codegen and have caller pop arguments, like cdecl.

@@overflow:
        JMP     _IntOver
end;
{$IFEND}

{$IF defined(MACOS) and defined(PUREPASCAL)}
//  StrRec = packed record
//    codePage: Word;
//    elemSize: Word;
//    refCnt: Longint;
//    length: Longint;
//  end;
function strEltSize(const s: UnicodeString): Integer; inline;
begin
   Result := PStrRec(Pointer(NativeInt(s) - SizeOf(StrRec)))^.elemSize;
end;

function strRawLength(const s: UnicodeString): Longint; inline;
begin
//   Result := PStrRec(Pointer(NativeInt(s) - SizeOf(StrRec)))^.length;
   Result := PLongint(Pointer(NativeInt(s) - SizeOf(Longint)))^;
end;

function StrRecIsNil(const p: PStrRec): Boolean; inline;
begin
   Result := NativeInt(p) = NativeInt(-SizeOf(StrRec));
end;

function _UStrCmpAsymmetric(const left, right: UnicodeString): Integer;
begin
   // this case should only arise in the face of C++ code
   Error(rePlatformNotImplemented);
   Result := 0;
end;

function _UStrCmpInternal(const left, right: UnicodeString): Integer;
var
   LengthDelta: Integer;
   leftR: PStrRec;
   rightR: PStrRec;
   compareLength: Integer;
begin
   if Pointer(left) = Pointer(right) then
      Exit(0);

   leftR := PStrRec(NativeInt(Pointer(left)) - SizeOf(StrRec));
   rightR := PStrRec(NativeInt(Pointer(right)) - SizeOf(StrRec));
   if StrRecIsNil(leftR) then
   begin
//      if StrRecIsNil(rightR) then
//         Exit(0)
//      else
//         Exit(leftR^.length);
      Exit(1);
   end
   else if StrRecIsNil(rightR) then
//      Exit(-leftR^.length)
      Exit(-1)
   else
   begin
      // both strings are present
      LengthDelta := leftR^.length - rightR^.length;
      if LengthDelta <> 0 then
         Exit(LengthDelta)
      else
      begin
         // both strings same length - now the awful payload escape
          // plain old unicode strings
          compareLength := leftR^.length;
          if compareLength > rightR^.length then
             compareLength := rightR^.length;
          Result := memcmp(Pointer(NativeInt(leftR) + SizeOf(StrRec))^,
                           Pointer(NativeInt(rightR) + SizeOf(StrRec))^,
                           compareLength);
      end;
    end;
end;
{$IFEND MACOS and PUREPASCAL}

{Original code by Pierre le Riche. Licensed under the CodeGear license terms.}
{$IF not defined(CPU386)}
function _UStrCmp(const Left, Right: UnicodeString): Integer;
var
  Len, LLen, RLen: Integer;
  LPtr, RPtr: PWideChar;
begin
  if Pointer(Left) = Pointer(Right) then
    Result := 0
  else if Pointer(Left) = nil then
    Result := 0 - __StringLength(Right)
  else if Pointer(Right) = nil then
    Result := __StringLength(Left)
  else
  begin
    LLen := __StringLength(Left);
    RLen := __StringLength(Right);
    Len := LLen;
    if Len > RLen then Len := RLen;
    LPtr := PWideChar(Left);
    RPtr := PWideChar(Right);
    while Len > 0 do
    begin
      Result := Ord(LPtr^) - Ord(RPtr^);
      if Result <> 0 then
        break;
      if Len = 1 then break;
      Result := Ord(LPtr[1]) - Ord(RPtr[1]);
      if Result <> 0 then
        break;
      Inc(LPtr, 2);
      Inc(RPtr, 2);
      Dec(Len, 2);
    end;
    if Len = 0 then
      Result := LLen - RLen;
  end;
end;
{$ELSE}


procedure _UStrCmp{const Left, Right: UnicodeString};
asm
  {On entry:
     eax = @Left[1]
     edx = @Right[1]
   On exit:
     Result in flags:
       CF = 1 if Left < Right, CF = 0 otherwise
       ZF = 1 if Left = Right, ZF = 0 otherwise}

        CMP     EAX, EDX // Do Left and Right point to the same string data?
        JE      @DoneNoPop

        TEST    EAX, EDX // Is one of the two string pointers perhaps nil?
        JZ      @PossibleNilString
@BothStringsNonNil:
  {Compare the first two characters. (There has to be a trailing #0, and non-nil
   UnicodeStrings must contain at least one other character so this comparison
   is safe). In "random" string compares this can save significant CPU time.}
        MOV     ECX, [EAX]
        CMP     ECX, [EDX]
        JNE @InitialMismatch

        PUSH    EBX             // Save ebx
        MOV     EBX, [EAX - 4]  // set ebx = Length(Left)
        XOR     ECX, ECX
        SUB     EBX, [EDX - 4]  // set ebx = Length(Left) - Length(Right)
        PUSH    EBX             // Save the length difference on the stack

        ADC     ECX, -1  // set ecx = 0 if Length(Left) < Length(Right), $ffffffff otherwise
        AND     ECX, EBX // set ecx = - min(length(Left), Length(Right))
        SUB     ECX, [EAX - 4]

        ADD     ECX, ECX        // Two bytes per character

        SUB     EAX, ECX        // Adjust the pointers to be
        SUB     EDX, ECX        // negative offset based

@CompareLoop:
        ADD     ECX, 4          // Next four bytes
        JNS     @MatchUpToLength
        {Compare four bytes (two characters) per cycle. This compare may include the
         trailing #0 for uneven string lengths, in which case no harm is done.}
        MOV     EBX, [EAX + ECX]
        CMP     EBX, [EDX + ECX]
        JE      @CompareLoop
        {Found a mismatch: Swap the two characters into the correct order}
        MOV     EDX, [EDX + ECX]
        ROR     EBX, 16
        ROR     EDX, 16

        CMP     EBX, EDX  // Compare the characters again, setting the flags
        POP     EBX       // Pop the length difference, restore ebx and return
        POP     EBX
        RET
        {All characters match up to the compare length}
@MatchUpToLength:
        POP     EAX       // Restore the string length difference to eax

        ADD     EAX, EAX  // Set the flags according to the length difference
        POP     EBX       // Restore ebx and return
@DoneNoPop:
        RET
@InitialMismatch:
        MOV     EDX, [EDX]  // Swap the two characters into the correct order
        ROR     ECX, 16
        ROR     EDX, 16
        CMP     ECX, EDX    // Compare them again so the flags are set correctly
        RET
@PossibleNilString:
        {There is a good probability that one of the strings are nil (but not both)}
        TEST    EAX, EAX
        JZ      @StringNil
        TEST    EDX, EDX
        JNZ     @BothStringsNonNil
@StringNil:
        CMP     EAX, EDX  // One of the strings are nil, so compare pointers
        RET
end;
{$IFEND}

{$IF not defined(CPU386)}
function _UStrEqual(const Left, Right: UnicodeString): Integer;
begin
  Result := _UStrCmp(Left, Right);
end;
{$ELSE}


{Original code by Pierre le Riche. Licensed under the CodeGear license terms.}
procedure _UStrEqual{const Left, Right: UnicodeString};
asm
  {On entry:
     eax = @Left[1]
     edx = @Right[1],
   On exit:
     ZF = 1 if Left = Right
     ZF = 0 if Left <> Right}

        CMP     EAX, EDX  // Same string?
        JE      @DoneNoPop

        TEST    EAX, EDX  // Any of the two possibly nil?
        JZ      @PossibleNilString
@BothStringsNonNil:

        MOV     ECX, [EAX - skew].StrRec.length // Get the string length
        CMP     ECX, [EDX - skew].StrRec.length // Are the string lengths the same?
        JNE     @DoneNoPop
        ADD     ECX, ECX    // Two bytes per character
        ADD     EAX, ECX    // Point eax and edx to just past the last character
        ADD     EDX, ECX
        NEG     ECX         // Make the counter negative based

        PUSH    EBX         // Save ebx
@CompareLoop:
        MOV     EBX, [EAX + ECX] // Compare four bytes per iteration
        CMP     EBX, [EDX + ECX]
        JNE     @Mismatch

        ADD     ECX, 4      // Next two characters
        JS      @CompareLoop
        XOR     EAX, EAX    // Match: Set the ZF
@Mismatch:
        POP     EBX         // Restore ebx

        RET
@PossibleNilString:
        {There is a good probability that one of the strings are nil (but not both)}
        TEST    EAX, EAX
        JZ      @StringNil
        TEST    EDX, EDX
        JNZ     @BothStringsNonNil
@StringNil:
        {One of the strings are nil. Clear the ZF.}
        CMP     EAX, EDX
@DoneNoPop:
end;
{$IFEND}

function _UStrCopy(const S: UnicodeString; Index, Count: Integer): UnicodeString;
var
  L, N: Integer;
begin
  L := Length(S);
  if Index < 1 then Index := 0 else
  begin
    Dec(Index);
    if Index > L then Index := L;
  end;
  if Count < 0 then N := 0 else
  begin
    N := L - Index;
    if N > Count then N := Count;
  end;
    _UStrFromPWCharLen(Result, PWideChar(Pointer(S)) + Index, N)
end;




procedure UStrSet(var S: UnicodeString; P: PWideChar);
{$IFDEF PUREPASCAL}
var
  Temp: Pointer;
begin
  Temp := Pointer(InterlockedExchange(Pointer(S), Pointer(P)));
  if Temp <> nil then
    _UStrClr(Temp);
end;
{$ELSE}
asm
        XCHG    [EAX],EDX
        TEST    EDX,EDX
        JZ      @@1
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EDX
        MOV     EAX,ESP
        CALL    _UStrClr
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
@@1:
end;
{$ENDIF !PUREPASCAL}

procedure _UStrDelete(var S: UnicodeString; Index, Count: Integer);
var
  L, N: Integer;
begin
  InternalUniqueStringU(S);
  L := Length(S);
  if (Index >= 1) and (Index <= L) and (Count > 0) then
  begin
    Dec(Index);
    N := L - Index - Count;
    if N < 0 then
      N := 0;
    Move(PWideChar(Pointer(S))[L - N], PWideChar(Pointer(S))[Index], N * 2);
    SetLength(S, Index + N);
  end;
end;

procedure _UStrInsert(const Source: UnicodeString; var Dest: UnicodeString; Index: Integer);
var
  SourceLen, DestLen, NewLen: Integer;
  SelfInsert: Boolean;
begin
  SourceLen := Length(Source);
  if SourceLen > 0 then
  begin
    DestLen := Length(Dest);
    if Index < 1 then Index := 0 else
    begin
      Dec(Index);
      if Index > DestLen then Index := DestLen;
    end;
    SelfInsert := (Pointer(Source) = Pointer(Dest));
    NewLen := DestLen + SourceLen;
    if NewLen < 0 then   // overflow check
      _IntOver;
    SetLength(Dest, NewLen);
    if Index < DestLen then
      Move(PWideChar(Pointer(Dest))[Index], PWideChar(Pointer(Dest))[Index + SourceLen],
        (DestLen - Index) * 2);
    if SelfInsert then
      Move(Pointer(Dest)^, PWideChar(Pointer(Dest))[Index], SourceLen * 2)
    else
      Move(Pointer(Source)^, PWideChar(Pointer(Dest))[Index], SourceLen * 2);
  end;
end;

{ ----------------------------------------------------- }
{       string utilities                                }
{ ----------------------------------------------------- }

function Pos(const SubStr, Str: ShortString): Integer;
{$IF not defined(CPU386)}
var
  SubLen, SrcLen, Len, I, J: Integer;
  C1: AnsiChar;
begin
  SrcLen := Byte(Str[0]);
  SubLen := Byte(SubStr[0]);
  Result := 0;
  if (SubLen <= 0) or (SrcLen <= 0) or (SrcLen < SubLen) then Exit;
  // find SubStr[1] in Str[1 .. SrcLen - SubLen + 1]
  Len := SrcLen - SubLen + 1;
  C1 := SubStr[1];
  for I := 1 to Len do
  begin
    if Str[I] = C1 then
    begin
      Result := I;
      for J := 1 to SubLen-1 do
      begin
        if Str[I+J] <> SubStr[1+J] then
        begin
          Result := 0;
          break;
        end;
      end;
      if Result <> 0 then Exit;
    end;
  end;
  // not found
end;
{$ELSE}


asm
        {     ->EAX     Pointer to substr               }
        {       EDX     Pointer to string               }
        {     <-EAX     Position of substr in s or 0    }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX         { Point ESI to substr                   }
        MOV     EDI,EDX         { Point EDI to s                        }

        XOR     ECX,ECX         { ECX = Length(s)                       }
        MOV     CL,[EDI]
        INC     EDI             { Point EDI to first char of s          }

        PUSH    EDI             { remember s position to calculate index}

        XOR     EDX,EDX         { EDX = Length(substr)                  }
        MOV     DL,[ESI]
        INC     ESI             { Point ESI to first char of substr     }

        DEC     EDX             { EDX = Length(substr) - 1              }
        JS      @@fail          { < 0 ? return 0                        }
        MOV     AL,[ESI]        { AL = first char of substr             }
        INC     ESI             { Point ESI to 2'nd char of substr      }

        SUB     ECX,EDX         { #positions in s to look at            }
                                { = Length(s) - Length(substr) + 1      }
        JLE     @@fail
@@loop:
        REPNE   SCASB
        JNE     @@fail
        MOV     EBX,ECX         { save outer loop counter               }
        PUSH    ESI             { save outer loop substr pointer        }
        PUSH    EDI             { save outer loop s pointer             }

        MOV     ECX,EDX
        REPE    CMPSB
        POP     EDI             { restore outer loop s pointer          }
        POP     ESI             { restore outer loop substr pointer     }
        JE      @@found
        MOV     ECX,EBX         { restore outer loop counter            }
        JMP     @@loop

@@fail:
        POP     EDX             { get rid of saved s pointer            }
        XOR     EAX,EAX
        JMP     @@exit

@@found:
        POP     EDX             { restore pointer to first char of s    }
        MOV     EAX,EDI { EDI points of char after match        }
        SUB     EAX,EDX { the difference is the correct index   }
@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$IFEND}




function Pos(const SubStr, Str: UnicodeString): Integer; overload;
{$IF not defined(CPU386)}
var
  SubLen, SrcLen, Len, I, J: Integer;
  C1: WideChar;
begin
  Result := 0;
  if (Pointer(SubStr) = nil) or (Pointer(Str) = nil) then Exit;
  SrcLen := __StringLength(Str);
  SubLen := __StringLength(SubStr);
  if (SubLen <= 0) or (SrcLen <= 0) or (SrcLen < SubLen) then Exit;
  // find SubStr[1] in S[1 .. SrcLen - SubLen + 1]
  Len := SrcLen - SubLen + 1;
  C1 := PWideChar(SubStr)[0];
  for I := 0 to Len - 1 do
  begin
    if PWideChar(Str)[I] = C1 then
    begin
      Result := I + 1;
      for J := 1 to SubLen-1 do
      begin
        if PWideChar(Str)[I+J] <> PWideChar(SubStr)[J] then
        begin
          Result := 0;
          break;
        end;
      end;
      if Result <> 0 then
        break;
    end;
  end;
end;
{$ELSE}
asm
        {     ->EAX     Pointer to substr               }
        {       EDX     Pointer to string               }
        {     <-EAX     Position of substr in str or 0  }

        TEST    EAX,EAX
        JE      @@noWork

        TEST    EDX,EDX
        JE      @@stringEmpty

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX                         { Point ESI to substr           }
        MOV     EDI,EDX                         { Point EDI to s                }

        MOV     ECX,[EDI-4]                     { ECX = Length(s)               }

        PUSH    EDI                             { remember s position to calculate index        }

        MOV     EDX,[ESI-4]                     { EDX = Length(substr)          }

        DEC     EDX                             { EDX = Length(substr) - 1              }
        JS      @@fail                          { < 0 ? return 0                        }
        MOV     AX,[ESI]                        { AL = first char of substr             }
        ADD     ESI,2                           { Point ESI to 2'nd char of substr      }

        SUB     ECX,EDX                         { #positions in s to look at    }
                                                { = Length(s) - Length(substr) + 1      }
        JLE     @@fail
@@loop:
        REPNE   SCASW
        JNE     @@fail
        MOV     EBX,ECX                         { save outer loop counter               }
        PUSH    ESI                             { save outer loop substr pointer        }
        PUSH    EDI                             { save outer loop s pointer             }

        MOV     ECX,EDX
        REPE    CMPSW
        POP     EDI                             { restore outer loop s pointer  }
        POP     ESI                             { restore outer loop substr pointer     }
        JE      @@found
        MOV     ECX,EBX                         { restore outer loop counter    }
        JMP     @@loop

@@fail:
        POP     EDX                             { get rid of saved s pointer    }
        XOR     EAX,EAX
        JMP     @@exit

@@stringEmpty:
        XOR     EAX,EAX
        JMP     @@noWork

@@found:
        POP     EDX                             { restore pointer to first char of s    }
        MOV     EAX,EDI                         { EDI points of char after match        }
        SUB     EAX,EDX                         { the difference is the correct index   }
        SHR     EAX,1
@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
@@noWork:
end;
{$IFEND}




function Pos(const SubStr, Str: WideString): Integer; overload;
{$IFDEF MSWINDOWS}
{$IF not defined(CPU386)}
var
  SubLen, SrcLen, Len, I, J: Integer;
  C1: WideChar;
begin
  Result := 0;
  if (Pointer(SubStr) = nil) or (Pointer(Str) = nil) then Exit;
  SrcLen := __StringLength(Str);
  SubLen := __StringLength(SubStr);
  if (SubLen <= 0) or (SrcLen <= 0) or (SrcLen < SubLen) then Exit;
  // find SubStr[1] in S[1 .. SrcLen - SubLen + 1]
  Len := SrcLen - SubLen + 1;
  C1 := PWideChar(SubStr)[0];
  for I := 0 to Len - 1 do
  begin
    if PWideChar(Str)[I] = C1 then
    begin
      Result := I + 1;
      for J := 1 to SubLen-1 do
      begin
        if PWideChar(Str)[I+J] <> PWideChar(SubStr)[J] then
        begin
          Result := 0;
          break;
        end;
      end;
      if Result <> 0 then
        break;
    end;
  end;
end;
{$ELSE CPU386}
asm //StackAlignSafe
        {     ->EAX     Pointer to substr               }
        {       EDX     Pointer to string               }
        {     <-EAX     Position of substr in str or 0  }

        TEST    EAX,EAX
        JE      @@noWork

        TEST    EDX,EDX
        JE      @@stringEmpty

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX                         { Point ESI to substr           }
        MOV     EDI,EDX                         { Point EDI to s                }

        MOV     ECX,[EDI-4]                     { ECX = Length(s)               }
        SHR     ECX,1

        PUSH    EDI                             { remember s position to calculate index        }

        MOV     EDX,[ESI-4]                     { EDX = Length(substr)          }
        SHR     EDX,1

        DEC     EDX                             { EDX = Length(substr) - 1              }
        JS      @@fail                          { < 0 ? return 0                        }
        MOV     AX,[ESI]                        { AL = first char of substr             }
        ADD     ESI,2                           { Point ESI to 2'nd char of substr      }

        SUB     ECX,EDX                         { #positions in s to look at    }
                                                { = Length(s) - Length(substr) + 1      }
        JLE     @@fail
@@loop:
        REPNE   SCASW
        JNE     @@fail
        MOV     EBX,ECX                         { save outer loop counter               }
        PUSH    ESI                             { save outer loop substr pointer        }
        PUSH    EDI                             { save outer loop s pointer             }

        MOV     ECX,EDX
        REPE    CMPSW
        POP     EDI                             { restore outer loop s pointer  }
        POP     ESI                             { restore outer loop substr pointer     }
        JE      @@found
        MOV     ECX,EBX                         { restore outer loop counter    }
        JMP     @@loop

@@fail:
        POP     EDX                             { get rid of saved s pointer    }
        XOR     EAX,EAX
        JMP     @@exit

@@stringEmpty:
        XOR     EAX,EAX
        JMP     @@noWork

@@found:
        POP     EDX                             { restore pointer to first char of s    }
        MOV     EAX,EDI                         { EDI points of char after match        }
        SUB     EAX,EDX                         { the difference is the correct index   }
        SHR     EAX,1
@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
@@noWork:
end;
{$IFEND CPU386}
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  Result := Pos(UnicodeString(Pointer(SubStr)), UnicodeString(Pointer(Str)));
end;
{$ENDIF}

function Pos(const SubStr, Str: RawByteString): Integer; overload;
{$IF not defined(CPU386)}

var
  SubLen, SrcLen, Len, I, J: Integer;
  C1: AnsiChar;
begin
  Result := 0;
  if (Pointer(SubStr) = nil) or (Pointer(Str) = nil) then Exit;
  SrcLen := __StringLength(Str);
  SubLen := __StringLength(SubStr);
  if (SubLen <= 0) or (SrcLen <= 0) or (SrcLen < SubLen) then Exit;
  // find SubStr[1] in Str[1 .. SrcLen - SubLen + 1]
  Len := SrcLen - SubLen + 1;
  C1 := PAnsiChar(SubStr)[0];
  for I := 0 to Len - 1 do
  begin
    if PAnsiChar(Str)[I] = C1 then
    begin
      Result := I + 1;
      for J := 1 to SubLen-1 do
      begin
        if PAnsiChar(Str)[I+J] <> PAnsiChar(SubStr)[J] then
        begin
          Result := 0;
          break;
        end;
      end;
      if Result <> 0 then
        break;
    end;
  end;
end;
{$ELSE}


(* ***** BEGIN LICENSE BLOCK *****
 *
 * The function Pos is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): Aleksandr Sharahov
 *
 * ***** END LICENSE BLOCK ***** *)
asm //StackAlignSafe
       PUSH  EBX
       PUSH  ESI
       ADD   ESP, -16
       TEST  EDX, EDX
       JZ    @NotFound
       TEST  EAX, EAX
       JZ    @NotFound
       MOV   ESI, [EDX-4] //Length(Str)
       MOV   EBX, [EAX-4] //Length(Substr)
       CMP   ESI, EBX
       JL    @NotFound
       TEST  EBX, EBX
       JLE   @NotFound
       DEC   EBX
       ADD   ESI, EDX
       ADD   EDX, EBX
       MOV   [ESP+8], ESI
       ADD   EAX, EBX
       MOV   [ESP+4], EDX
       NEG   EBX
       MOVZX ECX, BYTE PTR [EAX]
       MOV   [ESP], EBX
       JNZ   @FindString

       SUB   ESI, 2
       MOV   [ESP+12], ESI

@FindChar2:
       CMP   CL, [EDX]
       JZ    @Matched0ch
       CMP   CL, [EDX+1]
       JZ    @Matched1ch
       ADD   EDX, 2
       CMP   EDX, [ESP+12]
       JB    @FindChar4
       CMP   EDX, [ESP+8]
       JB    @FindChar2
@NotFound:
       XOR   EAX, EAX
       JMP   @Exit0ch

@FindChar4:
       CMP   CL, [EDX]
       JZ    @Matched0ch
       CMP   CL, [EDX+1]
       JZ    @Matched1ch
       CMP   CL, [EDX+2]
       JZ    @Matched2ch
       CMP   CL, [EDX+3]
       JZ    @Matched3ch
       ADD   EDX, 4
       CMP   EDX, [ESP+12]
       JB    @FindChar4
       CMP   EDX, [ESP+8]
       JB    @FindChar2
       XOR   EAX, EAX
       JMP   @Exit0ch

@Matched2ch:
       ADD   EDX, 2
@Matched0ch:
       INC   EDX
       MOV   EAX, EDX
       SUB   EAX, [ESP+4]
@Exit0ch:
       ADD   ESP, 16
       POP   ESI
       POP   EBX
       RET

@Matched3ch:
       ADD   EDX, 2
@Matched1ch:
       ADD   EDX, 2
       XOR   EAX, EAX
       CMP   EDX, [ESP+8]
       JA    @Exit1ch
       MOV   EAX, EDX
       SUB   EAX, [ESP+4]
@Exit1ch:
       ADD   ESP, 16
       POP   ESI
       POP   EBX
       RET

@FindString4:
       CMP   CL, [EDX]
       JZ    @Test0
       CMP   CL, [EDX+1]
       JZ    @Test1
       CMP   CL, [EDX+2]
       JZ    @Test2
       CMP   CL, [EDX+3]
       JZ    @Test3
       ADD   EDX, 4
       CMP   EDX, [ESP+12]
       JB    @FindString4
       CMP   EDX, [ESP+8]
       JB    @FindString2
       XOR   EAX, EAX
       JMP   @Exit1

@FindString:
       SUB   ESI, 2
       MOV   [ESP+12], ESI
@FindString2:
       CMP   CL, [EDX]
       JZ    @Test0
@AfterTest0:
       CMP   CL, [EDX+1]
       JZ    @Test1
@AfterTest1:
       ADD   EDX, 2
       CMP   EDX, [ESP+12]
       JB    @FindString4
       CMP   EDX, [ESP+8]
       JB    @FindString2
       XOR   EAX, EAX
       JMP   @Exit1

@Test3:
       ADD   EDX, 2
@Test1:
       MOV   ESI, [ESP]
@Loop1:
       MOVZX EBX, WORD PTR [ESI+EAX]
       CMP   BX, WORD PTR [ESI+EDX+1]
       JNZ   @AfterTest1
       ADD   ESI, 2
       JL    @Loop1
       ADD   EDX, 2
       XOR   EAX, EAX
       CMP   EDX, [ESP+8]
       JA    @Exit1
@RetCode1:
       MOV   EAX, EDX
       SUB   EAX, [ESP+4]
@Exit1:
       ADD   ESP, 16
       POP   ESI
       POP   EBX
       RET

@Test2:
       ADD   EDX,2
@Test0:
       MOV   ESI, [ESP]
@Loop0:
       MOVZX EBX, WORD PTR [ESI+EAX]
       CMP   BX, WORD PTR [ESI+EDX]
       JNZ   @AfterTest0
       ADD   ESI, 2
       JL    @Loop0
       INC   EDX
@RetCode0:
       MOV   EAX, EDX
       SUB   EAX, [ESP+4]
       ADD   ESP, 16
       POP   ESI
       POP   EBX
end;
{$IFEND}

function StringOfChar(Ch: WideChar; Count: Integer): UnicodeString; overload;
var
  P: PWideChar;
begin
  _UStrFromPWCharLen(Result, nil, Count);
  P := Pointer(Result);
  while Count > 0 do
  begin
    Dec(Count);
    P[Count] := Ch;
  end;
end;




function StringOfChar(Ch: AnsiChar; Count: Integer): AnsiString; overload;
{$IF not defined(CPU386)}
begin
  _LStrClr(Result);
  if Count > 0 then
  begin
    Pointer(Result) := _NewAnsiString(Count, DefaultSystemCodePage);
    _FillChar(Pointer(Result)^, Count, Ch);
  end;
end;
{$ELSE}
asm //StackAligned
        { ->    AL      c               }
        {       EDX     count           }
        {       ECX     result          }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        MOV     EAX,ECX
        CALL    _LStrClr

        TEST    ESI,ESI
        JLE     @@exit

        MOV     EAX,ESI
{$IFDEF PIC}
        PUSH    EAX
        PUSH    EBX
        PUSH    ECX
        CALL    GetGOT
        MOV     EDX, [EAX].OFFSET DefaultSystemCodePage
        MOV     EDX, [EDX]
        POP     ECX
        POP     EBX
        POP     EAX
{$ELSE}
        MOV     EDX,DefaultSystemCodePage
{$ENDIF}
        CALL    _NewAnsiString

        MOV     [EDI],EAX

        MOV     EDX,ESI
        MOV     CL,BL

        CALL    _FillChar

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX

end;
{$IFEND}

procedure SetAnsiString(Dest: PAnsiString; Source: PWideChar; Length: Integer; CodePage: Word);
begin
  _LStrFromPWCharLen(Dest^, Source, Length, CodePage);
end;

procedure SetCodePage(var S: RawByteString; CodePage: Word; Convert: Boolean);
var
  W: UnicodeString;
  NewLen: Integer;
begin
  if (StringCodePage(S) = CodePage) or (Length(S) = 0) then
    Exit;
  if Convert then
  begin
    if StringElementSize(S) = 1 then
      W := UnicodeString(S)  // This up-converts to Unicode utf-16 using the existing codepage in the payload
    else
      W := UnicodeString(Pointer(S));  // Payload is already utf-16 so just reference it
    // now find out how large the resulting string will be
    NewLen := CharFromWChar(nil, 0, PWideChar(W), Length(W), CodePage);
    SetLength(S, NewLen);
    // finally actually convert the payload based on the new CodePage
    if NewLen > 0 then
      CharFromWChar(PAnsiChar(S), Length(S), PWideChar(W), Length(W), CodePage);
  end
  else
    InternalUniqueStringA(AnsiString(S));
  if Length(S) > 0 then
    PWord(NativeInt(S) - 12)^ := CodePage;
end;

function UnicodeStringToUCS4String(const S: UnicodeString): UCS4String;
var
  I: Integer;
  CharCount: Integer;
begin
  CharCount := 0;
  SetLength(Result, Length(S) + 1);
  I := 0;
  while I < Length(S) do
  begin

    if ((S[I + 1] >= #$D800) and (S[I + 1] <= #$DFFF)) and (I + 1 < Length(S)) then
    begin
      Result[CharCount] := UCS4Char((Cardinal(S[I + 1]) and $000003FF) shl 10 or (Cardinal(S[I + 2]) and $000003FF) + $00010000);
      Inc(I);
    end
    else
      Result[CharCount] := UCS4Char(S[I + 1]);

    Inc(CharCount);
    Inc(I);
  end;
  Result[CharCount] := 0;
  SetLength(Result, CharCount + 1);
end;

function UCS4StringToUnicodeString(const S: UCS4String): UnicodeString;
var
  I: Integer;
  CharCount: Integer;
begin
  SetLength(Result, Length(S) * 2 - 1); //Maximum possible number of characters
  CharCount := 0;

  I := 0;
  while I < Length(S) - 1 do
  begin
    if S[I] >= $10000 then
    begin
      Inc(CharCount);
      Result[CharCount] := WideChar((((S[I] - $00010000) shr 10) and $000003FF) or $D800);
      Inc(CharCount);
      Result[CharCount] := WideChar(((S[I] - $00010000) and $000003FF)or $DC00);
    end
    else
    begin
      Inc(CharCount);
      Result[CharCount] := WideChar(S[I]);
    end;

    Inc(I);
  end;

  SetLength(Result, CharCount);
end;

function WideCharToUCS4String(S: PWideChar; Len: Integer = MaxInt): UCS4String;
var
  Buffer: array[0..255] of UCS4Char;
  Index: Integer;

  procedure FlushBuffer(var Result: UCS4String; AddNull: Integer);
  begin
    SetLength(Result, Length(Result) + Index + AddNull);
    Move(Buffer, Result[Length(Result) - Index - AddNull], Index * SizeOf(UCS4Char));
    if AddNull > 0 then
      Result[Length(Result) - 1] := 0;
    Index := 0;
  end;

begin
  Index := 0;
  while (S[0] <> #0) and (Len > 0) do
  begin
    if ((S[0] >= #$D800) and (S[0] <= #$DFFF)) and (Len > 0) and (S[1] <> #0) then
    begin
      Buffer[Index] := UCS4Char((Cardinal(S[0]) and $000003FF) shl 10 or (Cardinal(S[1]) and $000003FF) + $00010000);
      Inc(S);
    end
    else
      Buffer[Index] := UCS4Char(S[0]);
    Inc(Index);
    Inc(S);
    Dec(Len);
    if Index >= Length(Buffer) then
      FlushBuffer(Result, 0);
  end;
  FlushBuffer(Result, 1);
end;


//function UTF8Encode(const WS: UnicodeString): UTF8String;
//function UTF8Decode(const S: UTF8String): UnicodeString;


{ ------------------------------------------------------------- }
{       Compiler helper for initializing/finalizing variable    }
{ ------------------------------------------------------------- }

type
  PPTypeInfo = ^PTypeInfo;
  PTypeInfo = ^TTypeInfo;
  TTypeInfo = packed record
    Kind: Byte;
    Name: ShortString;
   {TypeData: TTypeData}
  end;

  // 32 bit = 8 bytes, 64bit = 16 bytes
  TFieldInfo = packed record
    TypeInfo: PPTypeInfo;
    case Integer of
    0: ( Offset: Cardinal );
    1: ( _Dummy: NativeUInt );
  end;

  PFieldTable = ^TFieldTable;
  TFieldTable = packed record
    X: Word;
    Size: Cardinal;
    Count: Cardinal;
    Fields: array [0..0] of TFieldInfo;
  end;

{ ===========================================================================
  InitializeRecord, InitializeArray, and Initialize are PIC safe even though
  they alter EBX because they only call each other.  They never call out to
  other functions and they don't access global data.

  FinalizeRecord, Finalize, and FinalizeArray are PIC safe because they call
  Pascal routines which will have EBX fixup prologs.
  ===========================================================================}



procedure   _InitializeRecord(p: Pointer; typeInfo: Pointer);
{$IFDEF PUREPASCAL}
var
  FT: PFieldTable;
  I: Cardinal;
begin
  FT := PFieldTable(NativeInt(typeInfo) + Byte(PTypeInfo(typeInfo).Name[0]));
  for I := FT.Count-1 downto 0 do
    _InitializeArray(Pointer(NativeInt(P) + NativeInt(FT.Fields[I].Offset)), FT.Fields[I].TypeInfo^, 1);
end;
{$ELSE}
asm
        { ->    EAX pointer to record to be initialized }
        {       EDX pointer to type info                }

        XOR     ECX,ECX

        PUSH    EBX
        MOV     CL,[EDX+1]                  { type name length }

        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX                     // PIC safe. See comment above
        LEA     ESI,[EDX+ECX+2+8]           { address of destructable fields }
        MOV     EDI,[EDX+ECX+2+4]           { number of destructable fields }
        TEST    EDI,EDI
        JZ      @@exit

@@loop:

        MOV     EDX,[ESI]
        MOV     EAX,[ESI+4]
        ADD     EAX,EBX
        MOV     EDX,[EDX]
        MOV     ECX,1
        CALL    _InitializeArray
        ADD     ESI,8
        DEC     EDI
        JG      @@loop

@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF !PUREPASCAL}


const
  tkLString   = 10;
  tkWString   = 11;
  tkVariant   = 12;
  tkArray     = 13;
  tkRecord    = 14;
  tkInterface = 15;
  tkDynArray  = 17;
  tkUString   = 18;
  tkMRecord   = 19;





procedure InitializeArray(p: Pointer; typeInfo: Pointer; elemCount: Cardinal);
{$IF not defined(CPU386)}
begin
  _InitializeArray(p, typeInfo, elemCount);
end;
{$ELSE}
asm
  JMP _InitializeArray
end;
{$IFEND}



procedure       _InitializeArray(p: Pointer; typeInfo: Pointer; elemCount: Cardinal);
{$IFDEF PUREPASCAL}

var
  FT: PFieldTable;
begin
  if elemCount = 0 then Exit;
  case PTypeInfo(typeInfo).Kind of
    tkLString, tkWString, tkInterface, tkDynArray, tkUString:
      while elemCount > 0 do
      begin
        PPointer(P)^ := nil;
        Inc(NativeInt(P), SizeOf(Pointer));
        Dec(elemCount);
      end;
    tkVariant:
      while elemCount > 0 do
      begin
        PLongInt(P)^ := 0;
        PLongInt(NativeInt(P)+4)^ := 0;
        PLongInt(NativeInt(P)+8)^ := 0;
        PLongInt(NativeInt(P)+12)^ := 0;
        Inc(NativeInt(P), SizeOf(Variant));
        Dec(elemCount);
      end;
    tkArray:
      begin
        FT := PFieldTable(NativeInt(typeInfo) + Byte(PTypeInfo(typeInfo).Name[0]));
        while elemCount > 0 do
        begin
          _InitializeArray(P, FT.Fields[0].TypeInfo^, FT.Count);
          Inc(NativeInt(P), FT.Size);
          Dec(elemCount);
        end;
      end;
    tkRecord:
      begin
        FT := PFieldTable(NativeInt(typeInfo) + Byte(PTypeInfo(typeInfo).Name[0]));
        while elemCount > 0 do
        begin
          _InitializeRecord(P, typeInfo);
          Inc(NativeInt(P), FT.Size);
          Dec(elemCount);
        end;
      end;
  else
    Error(reInvalidPtr);
  end;
end;
{$ELSE}
asm
        { ->    EAX     pointer to data to be initialized       }
        {       EDX     pointer to type info describing data    }
        {       ECX     number of elements of that type         }

        TEST    ECX, ECX
        JZ      @@zerolength

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        MOV     EBX,EAX             // PIC safe.  See comment above
        MOV     ESI,EDX
        MOV     EDI,ECX

        XOR     EDX,EDX
        MOV     AL,[ESI]
        MOV     DL,[ESI+1]
        XOR     ECX,ECX

        CMP     AL,tkLString
        JE      @@LString
        CMP     AL,tkWString
        JE      @@WString
        CMP     AL,tkVariant
        JE      @@Variant
        CMP     AL,tkArray
        JE      @@Array
        CMP     AL,tkRecord
        JE      @@Record
        CMP     AL,tkInterface
        JE      @@Interface
        CMP     AL,tkDynArray
        JE      @@DynArray
        CMP     AL,tkUString
        JE      @@UString
        MOV     AL,reInvalidPtr
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     Error

@@LString:
@@WString:
@@Interface:
@@DynArray:
@@UString:
        MOV     [EBX],ECX
        ADD     EBX,4
        DEC     EDI
        JG      @@LString
        JMP     @@exit

@@Variant:
        MOV     [EBX   ],ECX
        MOV     [EBX+ 4],ECX
        MOV     [EBX+ 8],ECX
        MOV     [EBX+12],ECX
        ADD     EBX,16
        DEC     EDI
        JG      @@Variant
        JMP     @@exit

@@Array:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EBP
        MOV     EBP,EDX
@@ArrayLoop:
        MOV     EDX,[ESI+EBP+2+8]    // address of destructable fields typeinfo
        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]      // size in bytes of the array data
        MOV     ECX,[ESI+EBP+2+4]    // number of destructable fields
        MOV     EDX,[EDX]
        CALL    _InitializeArray
        DEC     EDI
        JG      @@ArrayLoop
        POP     EBP
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        JMP     @@exit

@@Record:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EBP
        MOV     EBP,EDX
@@RecordLoop:
        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]
        MOV     EDX,ESI
        CALL    _InitializeRecord
        DEC     EDI
        JG      @@RecordLoop
        POP     EBP
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}

@@exit:

        POP     EDI
        POP     ESI
        POP     EBX
@@zerolength:
end;
{$ENDIF !PUREPASCAL}




procedure       _Initialize(p: Pointer; typeInfo: Pointer);
{$IFDEF PUREPASCAL}
begin
  _InitializeArray(p, typeInfo, 1);
end;
{$ELSE}
asm
        MOV     ECX,1
        JMP     _InitializeArray
end;
{$ENDIF !PUREPASCAL}




{$IFDEF PUREPASCAL}
function _FinalizeRecord(P: Pointer; TypeInfo: Pointer): Pointer;
var
  FT: PFieldTable;
  I: Cardinal;
begin
  FT := PFieldTable(NativeInt(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name[0]));
  for I := 0 to FT.Count - 1 do
    _FinalizeArray(Pointer(NativeInt(P) + NativeInt(FT.Fields[I].Offset)), FT.Fields[I].TypeInfo^, 1);
  Result := P;
end;
{$ELSE}
procedure _FinalizeRecord(p: Pointer; typeInfo: Pointer);
asm
        { ->    EAX pointer to record to be finalized   }
        {       EDX pointer to type info                }

        XOR     ECX,ECX

        PUSH    EBX
        MOV     CL,[EDX+1]

        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX
        LEA     ESI,[EDX+ECX+2+8]
        MOV     EDI,[EDX+ECX+2+4]
        TEST    EDI,EDI
        JZ      @@exit

@@loop:

        MOV     EDX,[ESI]
        MOV     EAX,[ESI+4]
        ADD     EAX,EBX
        MOV     EDX,[EDX]
        MOV     ECX,1
        CALL    _FinalizeArray
        ADD     ESI,8
        DEC     EDI
        JG      @@loop

@@exit:
        MOV     EAX,EBX

        POP     EDI
        POP     ESI
        POP     EBX
end;
{$ENDIF !PUREPASCAL}

procedure _VarClr(var v: TVarData);
begin
  if Assigned(VarClearProc) then
    VarClearProc(v)
  else
    Error(reVarInvalidOp);
end;



{$IFDEF PUREPASCAL}
function _FinalizeArray(P: Pointer; TypeInfo: Pointer; ElemCount: Cardinal): Pointer;
var
  FT: PFieldTable;
begin
  Result := P;
  if ElemCount = 0 then Exit;
  case PTypeInfo(TypeInfo).Kind of
    tkLString: _LStrArrayClr(P^, ElemCount);
    tkWString: _WStrArrayClr(P^, ElemCount);
    tkUString: _UStrArrayClr(P^, ElemCount);
    tkVariant:
      while ElemCount > 0 do
      begin
        _VarClr(PVarData(P)^);
        Inc(NativeInt(P), SizeOf(Variant));
        Dec(ElemCount);
      end;
    tkArray:
      begin
        FT := PFieldTable(NativeInt(typeInfo) + Byte(PTypeInfo(typeInfo).Name[0]));
        while ElemCount > 0 do
        begin
          _FinalizeArray(P, FT.Fields[0].TypeInfo^, FT.Count);
          Inc(NativeInt(P), FT.Size);
          Dec(ElemCount);
        end;
      end;
    tkRecord:
      begin
        FT := PFieldTable(NativeInt(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name[0]));
        while ElemCount > 0 do
        begin
          _FinalizeRecord(P, TypeInfo);
          Inc(NativeInt(P), FT.Size);
          Dec(ElemCount);
        end;
      end;
    tkInterface:
      while ElemCount > 0 do
      begin
        _IntfClear(IInterface(P^));
        Inc(NativeInt(P), SizeOf(Pointer));
        Dec(ElemCount);
      end;
    tkDynArray:
      while ElemCount > 0 do
      begin
        { The cast and dereference of P here is to fake out the call to
          _DynArrayClear.  That function expects a var parameter.  Our
          declaration says we got a non-var parameter, but because of
          the data type that got passed to us (tkDynArray), this isn't
          strictly true.  The compiler will have passed us a reference. }
        _DynArrayClear(PPointer(P)^, typeInfo);
        Inc(NativeInt(P), SizeOf(Pointer));
        Dec(ElemCount);
      end;
  else
    Error(reInvalidPtr);
  end;
end;
{$ELSE !PUREPASCAL}

procedure _FinalizeArray(P: Pointer; TypeInfo: Pointer; ElemCount: Cardinal);
asm
        { ->    EAX     pointer to data to be finalized         }
        {       EDX     pointer to type info describing data    }
        {       ECX     number of elements of that type         }

        { This code appears to be PIC safe.  The functions called from
          here either don't make external calls or call Pascal
          routines that will fix up EBX in their prolog code
          (FreeMem, VarClr, IntfClr).  }

        CMP     ECX, 0                        { no array -> nop }
        JE      @@zerolength

        PUSH    EAX
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        XOR     EDX,EDX
        MOV     AL,[ESI]
        MOV     DL,[ESI+1]

        CMP     AL,tkLString
        JE      @@LString

        CMP     AL,tkUString
        JE      @@UString

        CMP     AL,tkWString
        JE      @@WString

        CMP     AL,tkVariant
        JE      @@Variant

        CMP     AL,tkArray
        JE      @@Array

        CMP     AL,tkRecord
        JE      @@Record

        CMP     AL,tkInterface
        JE      @@Interface

        CMP     AL,tkDynArray
        JE      @@DynArray

        JMP     @@error

@@LString:
        CMP     ECX,1
        MOV     EAX,EBX
        JG      @@LStringArray
        CALL    _LStrClr
        JMP     @@exit
@@LStringArray:
        MOV     EDX,ECX
        CALL    _LStrArrayClr
        JMP     @@exit

@@WString:
        CMP     ECX,1
        MOV     EAX,EBX
        JG      @@WStringArray
        CALL    _WStrClr
        JMP     @@exit
@@WStringArray:
        MOV     EDX,ECX
        CALL    _WStrArrayClr
        JMP     @@exit

@@UString:
        CMP     ECX,1
        MOV     EAX,EBX
        JG      @@UStringArray
        CALL    _UStrClr
        JMP     @@exit
@@UStringArray:
        MOV     EDX,ECX
        CALL    _UStrArrayClr
        JMP     @@exit

@@Variant:
        MOV     EAX,EBX
        ADD     EBX,16
        CALL    _VarClr
        DEC     EDI
        JG      @@Variant
        JMP     @@exit
@@Array:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EBP
        MOV     EBP,EDX
@@ArrayLoop:
        MOV     EDX,[ESI+EBP+2+8]
        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]
        MOV     ECX,[ESI+EBP+2+4]
        MOV     EDX,[EDX]
        CALL    _FinalizeArray
        DEC     EDI
        JG      @@ArrayLoop
        POP     EBP
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        JMP     @@exit

@@Record:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH    EBP
        MOV     EBP,EDX
@@RecordLoop:
        { inv: EDI = number of array elements to finalize }

        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]
        MOV     EDX,ESI
        CALL    _FinalizeRecord
        DEC     EDI
        JG      @@RecordLoop
        POP     EBP
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        JMP     @@exit

@@Interface:
        MOV     EAX,EBX
        ADD     EBX,4
        CALL    _IntfClear
        DEC     EDI
        JG      @@Interface
        JMP     @@exit

@@DynArray:
        MOV     EAX,EBX
        MOV     EDX,ESI
        ADD     EBX,4
        CALL    _DynArrayClear
        DEC     EDI
        JG      @@DynArray
        JMP     @@exit

@@error:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        POP     EDI
        POP     ESI
        POP     EBX
        POP     EAX
        MOV     AL,reInvalidPtr
        JMP     Error

@@exit:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        POP     EDI
        POP     ESI
        POP     EBX
        POP     EAX
@@zerolength:
end;
{$ENDIF !PUREPASCAL}




{$IFDEF PUREPASCAL}
function _Finalize(p: Pointer; typeInfo: Pointer): Pointer;
begin
  Result := _FinalizeArray(p, typeInfo, 1);
end;
{$ELSE}
procedure _Finalize(p: Pointer; typeInfo: Pointer);
asm
        MOV     ECX,1
        JMP     _FinalizeArray
end;
{$ENDIF}




{$IF not defined(CPU386)}
procedure _AddRefRecord(P: Pointer; TypeInfo: Pointer);
var
  FT: PFieldTable;
  I: Cardinal;
begin
  FT := PFieldTable(NativeInt(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name[0]));
  for I := 0 to FT.Count - 1 do
    _AddRefArray(Pointer(NativeInt(P) + NativeInt(FT.Fields[I].Offset)), FT.Fields[I].TypeInfo^, 1);
end;
{$ELSE}
procedure       _AddRefRecord{ p: Pointer; typeInfo: Pointer };
asm
        { ->    EAX pointer to record to be referenced  }
        {       EDX pointer to type info        }

        XOR     ECX,ECX

        PUSH    EBX
        MOV     CL,[EDX+1]

        PUSH    ESI
        PUSH    EDI

        MOV     EBX,EAX
        LEA     ESI,[EDX+ECX+2+8]
        MOV     EDI,[EDX+ECX+2+4]

@@loop:

        MOV     EDX,[ESI]
        MOV     EAX,[ESI+4]
        ADD     EAX,EBX
        MOV     EDX,[EDX]
        MOV     ECX, 1
        CALL    _AddRefArray
        ADD     ESI,8
        DEC     EDI
        JG      @@loop

        POP     EDI
        POP     ESI
        POP     EBX
end;
{$IFEND}

procedure _VarAddRef(var v: TVarData);
begin
  if Assigned(VarAddRefProc) then
    VarAddRefProc(v)
  else
    Error(reVarInvalidOp);
end;

{$IF not defined(CPU386)}

procedure _AddRefArray(P: Pointer; TypeInfo: Pointer; ElemCount: Longint);
var
  FT: PFieldTable;
begin
  if ElemCount = 0 then Exit;
  case PTypeInfo(TypeInfo).Kind of
    tkLString:
      while ElemCount > 0 do
      begin
        _LStrAddRef(PPointer(P)^);
        Inc(NativeInt(P), SizeOf(Pointer));
        Dec(ElemCount);
      end;
    tkWString:
      while ElemCount > 0 do
      begin
        _WStrAddRef(PWideString(P)^);
        Inc(NativeInt(P), SizeOf(Pointer));
        Dec(ElemCount);
      end;
    tkUString:
      while ElemCount > 0 do
      begin
        _UStrAddRef(PPointer(P)^);
        Inc(NativeInt(P), SizeOf(Pointer));
        Dec(ElemCount);
      end;
    tkVariant:
      while ElemCount > 0 do
      begin
        _VarAddRef(PVarData(P)^);
        Inc(NativeInt(P), SizeOf(Variant));
        Dec(ElemCount);
      end;
    tkArray:
      begin
        FT := PFieldTable(NativeInt(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name[0]));
        while ElemCount > 0 do
        begin
          _AddRefArray(P, FT.Fields[0].TypeInfo^, FT.Count);
          Inc(NativeInt(P), FT.Size);
          Dec(ElemCount);
        end;
      end;
    tkRecord:
      begin
        FT := PFieldTable(NativeInt(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name[0]));
        while ElemCount > 0 do
        begin
          _AddRefRecord(P, TypeInfo);
          Inc(NativeInt(P), FT.Size);
          Dec(ElemCount);
        end;
      end;
    tkInterface:
      while ElemCount > 0 do
      begin
        _IntfAddRef(IInterface(P^));
        Inc(NativeInt(P), SizeOf(Pointer));
        Dec(ElemCount);
      end;
    tkDynArray:
      while ElemCount > 0 do
      begin
        _DynArrayAddRef(PPointer(P)^);
        Inc(NativeInt(P), SizeOf(Pointer));
        Dec(ElemCount);
      end;
  else
    Error(reInvalidPtr);
  end;
end;
{$ELSE}


procedure       _AddRefArray{ p: Pointer; typeInfo: Pointer; elemCount: Longint};
asm //StackAligned
        { ->    EAX     pointer to data to be referenced        }
        {       EDX     pointer to type info describing data    }
        {       ECX     number of elements of that type         }

        { This code appears to be PIC safe.  The functions called from
          here either don't make external calls (LStrAddRef, WStrAddRef) or
          are Pascal routines that will fix up EBX in their prolog code
          (VarAddRef, IntfAddRef).  }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        TEST  ECX,ECX
        JZ    @@exit

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX

        XOR     EDX,EDX
        MOV     AL,[ESI]
        MOV     DL,[ESI+1]

        CMP     AL,tkLString
        JE      @@LString
        CMP     AL,tkWString
        JE      @@WString
        CMP     AL,tkUString
        JE      @@UString
        CMP     AL,tkVariant
        JE      @@Variant
        CMP     AL,tkArray
        JE      @@Array
        CMP     AL,tkRecord
        JE      @@Record
        CMP     AL,tkInterface
        JE      @@Interface
        CMP     AL,tkDynArray
        JE      @@DynArray
        MOV     AL,reInvalidPtr
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     Error

@@LString:
@@UString:
{$IFDEF POSIX}
@@WString:
{$ENDIF POSIX}
        MOV     EAX,[EBX]
        ADD     EBX,4
        CALL    _LStrAddRef
        DEC     EDI
        JG      @@LString
        JMP     @@exit

{$IFDEF MSWINDOWS}
@@WString:
        MOV     EAX,EBX
        ADD     EBX,4
        CALL    _WStrAddRef
        DEC     EDI
        JG      @@WString
        JMP     @@exit
{$ENDIF MSWINDOWS}
@@Variant:
        MOV     EAX,EBX
        ADD     EBX,16
        CALL    _VarAddRef
        DEC     EDI
        JG      @@Variant
        JMP     @@exit

@@Array:
        PUSH    EBP
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        MOV     EBP,EDX
@@ArrayLoop:
        MOV     EDX,[ESI+EBP+2+8]
        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]
        MOV     ECX,[ESI+EBP+2+4]
        MOV     EDX,[EDX]
        CALL    _AddRefArray
        DEC     EDI
        JG      @@ArrayLoop
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        POP     EBP
        JMP     @@exit

@@Record:
        PUSH    EBP
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        MOV     EBP,EDX
@@RecordLoop:
        MOV     EAX,EBX
        ADD     EBX,[ESI+EBP+2]
        MOV     EDX,ESI
        CALL    _AddRefRecord
        DEC     EDI
        JG      @@RecordLoop
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        POP     EBP
        JMP     @@exit

@@Interface:
        MOV     EAX,[EBX]
        ADD     EBX,4
        CALL    _IntfAddRef
        DEC     EDI
        JG      @@Interface
        JMP     @@exit

@@DynArray:
        MOV     EAX,[EBX]
        ADD     EBX,4
        CALL    _DynArrayAddRef
        DEC     EDI
        JG      @@DynArray
@@exit:

        POP     EDI
        POP     ESI
        POP     EBX
end;
{$IFEND}

{$IF not defined(CPU386)}
procedure _AddRef(P: Pointer; TypeInfo: Pointer);
begin
  _AddRefArray(P, TypeInfo, 1);
end;
{$ELSE}


procedure       _AddRef{ p: Pointer; typeInfo: Pointer};
asm
        MOV     ECX,1
        JMP     _AddRefArray
end;
{$IFEND}

procedure _VarCopy(var Dest: TVarData; const Src: TVarData);
begin
  if Assigned(VarCopyProc) then
    VarCopyProc(Dest, Src)
  else
    Error(reVarInvalidOp);
end;

{$IF not defined(CPU386)}

procedure _CopyRecord(Dest, Source, TypeInfo: Pointer);
var
  FT, EFT: PFieldTable;
  I: Cardinal;
  Offset: Cardinal;
  FTypeInfo: PTypeInfo;
begin
  FT := PFieldTable(NativeInt(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name[0]));
  Offset := 0;
  for I := 0 to FT.Count - 1 do
  begin
    if FT.Fields[I].Offset > Offset then
      Move(Pointer(NativeInt(Source) + Offset)^,
           Pointer(NativeInt(Dest) + Offset)^,
           FT.Fields[I].Offset - Offset);
    Offset := FT.Fields[I].Offset;
    FTypeInfo := FT.Fields[I].TypeInfo^;
    case FTypeInfo.Kind of
      tkLString:
        begin
          _LStrAsg(PAnsiString(NativeInt(Dest) + Offset)^, PAnsiString(NativeInt(Source) + Offset)^);
          Inc(Offset, SizeOf(Pointer));
        end;
      tkWString:
        begin
          _WStrAsg(PWideString(NativeInt(Dest) + Offset)^, PWideString(NativeInt(Source) + Offset)^);
          Inc(Offset, SizeOf(Pointer));
        end;
      tkUString:
        begin
          _UStrAsg(PUnicodeString(NativeInt(Dest) + Offset)^, PUnicodeString(NativeInt(Source) + Offset)^);
          Inc(Offset, SizeOf(Pointer));
        end;
      tkVariant:
        begin
          _VarCopy(PVarData(NativeInt(Dest) + Offset)^, PVarData(NativeInt(Source) + Offset)^);
          Inc(Offset, SizeOf(Variant));
        end;
      tkArray:
        begin
          EFT := PFieldTable(NativeInt(FTypeInfo) + Byte(PTypeInfo(FTypeInfo).Name[0]));
          _CopyArray(Pointer(NativeInt(Dest) + Offset), Pointer(NativeInt(Source) + Offset),
                     EFT.Fields[0].TypeInfo^, EFT.Count);
          Inc(Offset, EFT.Size);
        end;
      tkRecord:
        begin
          EFT := PFieldTable(NativeInt(FTypeInfo) + Byte(PTypeInfo(FTypeInfo).Name[0]));
          _CopyRecord(Pointer(NativeInt(Dest) + Offset), Pointer(NativeInt(Source) + Offset),
                     FTypeInfo);
          Inc(Offset, EFT.Size);
        end;
      tkInterface:
        begin
          _IntfCopy(IInterface(PPointer(NativeInt(Dest) + Offset)^), IInterface(PPointer(NativeInt(Source) + Offset)^));
          Inc(Offset, SizeOf(Pointer));
        end;
      tkDynArray:
        begin
          _DynArrayAsg(PPointer(NativeInt(Dest) + Offset)^, PPointer(NativeInt(Source) + Offset),
                       FTypeInfo);
          Inc(Offset, SizeOf(Pointer));
        end;
    else
      Error(reInvalidPtr);
    end;
  end;
  if FT.Size > Offset then
    Move(Pointer(NativeInt(Source) + Offset)^,
         Pointer(NativeInt(Dest) + Offset)^,
         FT.Size - Offset);
end;
{$ELSE}


procedure _CopyRecord{ dest, source, typeInfo: Pointer };
asm
        { ->    EAX pointer to dest             }
        {       EDX pointer to source           }
        {       ECX pointer to typeInfo         }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EBX,EAX
        MOV     ESI,EDX

        XOR     EAX,EAX
        MOV     AL,[ECX+1]

        LEA     EDI,[ECX+EAX+2+8]
        MOV     EBP,[EDI-4]
        XOR     EAX,EAX
        MOV     ECX,[EDI-8]
        TEST    EBP,EBP
        JZ      @@moveWhole
        PUSH    ECX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
@@loop:
        MOV     ECX,[EDI+4]
        SUB     ECX,EAX
        JLE     @@nomove1
        MOV     EDX,EAX
        ADD     EAX,ESI
        ADD     EDX,EBX
        CALL    Move
@@noMove1:
        MOV     EAX,[EDI+4]

        MOV     EDX,[EDI]
        MOV     EDX,[EDX]
        MOV     CL,[EDX]

        CMP     CL,tkLString
        JE      @@LString
        CMP     CL,tkWString
        JE      @@WString
        CMP     CL,tkUString
        JE      @@UString
        CMP     CL,tkVariant
        JE      @@Variant
        CMP     CL,tkArray
        JE      @@Array
        CMP     CL,tkRecord
        JE      @@Record
        CMP     CL,tkInterface
        JE      @@Interface
        CMP     CL,tkDynArray
        JE      @@DynArray
        MOV     AL,reInvalidPtr
        POP     ECX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     Error

@@LString:
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _LStrAsg
        MOV     EAX,4
        JMP     @@common

@@UString:
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _UStrAsg
        MOV     EAX,4
        JMP     @@common

@@WString:
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _WStrAsg
        MOV     EAX,4
        JMP     @@common

@@Variant:
        LEA     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _VarCopy
        MOV     EAX,16
        JMP     @@common

@@Array:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8  // Negitive adjustment
{$ENDIF ALIGN_STACK}
        XOR     ECX,ECX
        MOV     CL,[EDX+1]
        PUSH    dword ptr [EDX+ECX+2]
        PUSH    dword ptr [EDX+ECX+2+4]
        MOV     ECX,[EDX+ECX+2+8]
        MOV     ECX,[ECX]
        LEA     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _CopyArray
        POP     EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        JMP     @@common

@@Record:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4  //Negitive adjustment
{$ENDIF ALIGN_STACK}
        XOR     ECX,ECX
        MOV     CL,[EDX+1]
        MOV     ECX,[EDX+ECX+2]
        PUSH    ECX
        MOV     ECX,EDX
        LEA     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _CopyRecord
        POP     EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        JMP     @@common

@@Interface:
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _IntfCopy
        MOV     EAX,4
        JMP     @@common

@@DynArray:
        MOV     ECX,EDX
        MOV     EDX,[ESI+EAX]
        ADD     EAX,EBX
        CALL    _DynArrayAsg
        MOV     EAX,4

@@common:
        ADD     EAX,[EDI+4]
        ADD     EDI,8
        DEC     EBP
        JNZ     @@loop
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        POP     ECX
@@moveWhole:
        SUB     ECX,EAX
        JLE     @@noMove2
        LEA     EDX,[EBX+EAX]
        ADD     EAX,ESI
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    Move
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
@@noMove2:

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$IFEND}




{$IF not defined(CPU386)}
procedure _CopyObject(Dest, Source: Pointer; vmtPtrOffs: LongInt; TypeInfo: Pointer);
var
  SavedVmtPtr: Pointer;
begin
  SavedVmtPtr := PPointer(NativeInt(dest) + vmtPtrOffs)^;
  _CopyRecord(dest, source, typeInfo);
  PPointer(NativeInt(dest) + vmtPtrOffs)^ := SavedVmtPtr;
end;
{$ELSE}
procedure       _CopyObject{ dest, source: Pointer; vmtPtrOffs: Longint; typeInfo: Pointer };
asm
        { ->    EAX     pointer to dest         }
        {       EDX     pointer to source       }
        {       ECX     offset of vmt in object }
        {       [ESP+4] pointer to typeInfo     }

        ADD     ECX,EAX                         { pointer to dest vmt }
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    dword ptr [ECX]                 { save dest vmt }
        PUSH    ECX
        MOV     ECX,[ESP+4+4+4]
        CALL    _CopyRecord
        POP     ECX
        POP     dword ptr [ECX]                 { restore dest vmt }
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
        RET     4

end;
{$IFEND}

{$IF not defined(CPU386)}

procedure _CopyArray(Dest, Source, TypeInfo: Pointer; Count: Integer);
var
  FT: PFieldTable;
begin
  if Count = 0 then Exit;
  case PTypeInfo(TypeInfo).Kind of
    tkLString:
      while Count > 0 do
      begin
        _LStrAsg(PAnsiString(Dest)^, PAnsiString(Source)^);
        Inc(NativeInt(Dest), SizeOf(Pointer));
        Inc(NativeInt(Source), SizeOf(Pointer));
        Dec(Count);
      end;
    tkWString:
      while Count > 0 do
      begin
        _WStrAsg(PWideString(Dest)^, PWideString(Source)^);
        Inc(NativeInt(Dest), SizeOf(Pointer));
        Inc(NativeInt(Source), SizeOf(Pointer));
        Dec(Count);
      end;
    tkUString:
      while Count > 0 do
      begin
        _UStrAsg(PUnicodeString(Dest)^, PUnicodeString(Source)^);
        Inc(NativeInt(Dest), SizeOf(Pointer));
        Inc(NativeInt(Source), SizeOf(Pointer));
        Dec(Count);
      end;
    tkVariant:
      while Count > 0 do
      begin
        _VarCopy(PVarData(Dest)^, PVarData(Source)^);
        Inc(NativeInt(Dest), SizeOf(Variant));
        Inc(NativeInt(Source), SizeOf(Variant));
        Dec(Count);
      end;
    tkArray:
      begin
        FT := PFieldTable(NativeInt(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name[0]));
        while Count > 0 do
        begin
          _CopyArray(Pointer(Dest), Pointer(Source), FT.Fields[0].TypeInfo^, FT.Count);
          Inc(NativeInt(Dest), FT.Size);
          Inc(NativeInt(Source), FT.Size);
          Dec(Count);
        end;
      end;
    tkRecord:
      begin
        FT := PFieldTable(NativeInt(TypeInfo) + Byte(PTypeInfo(TypeInfo).Name[0]));
        while Count > 0 do
        begin
          _CopyRecord(Pointer(Dest), Pointer(Source), TypeInfo);
          Inc(NativeInt(Dest), FT.Size);
          Inc(NativeInt(Source), FT.Size);
          Dec(Count);
        end;
      end;
    tkInterface:
      while Count > 0 do
      begin
        _IntfCopy(IInterface(PPointer(Dest)^), IInterface(PPointer(Source)^));
        Inc(NativeInt(Dest), SizeOf(Pointer));
        Inc(NativeInt(Source), SizeOf(Pointer));
        Dec(Count);
      end;
    tkDynArray:
      while Count > 0 do
      begin
        _DynArrayAsg(PPointer(Dest)^, PPointer(Source), TypeInfo);
        Inc(NativeInt(Dest), SizeOf(Pointer));
        Inc(NativeInt(Source), SizeOf(Pointer));
        Dec(Count);
      end;
  else
    Error(reInvalidPtr);
  end;
end;
{$ELSE}


procedure _CopyArray{ dest, source, typeInfo: Pointer; cnt: Integer };
asm
        { ->    EAX pointer to dest             }
        {       EDX pointer to source           }
        {       ECX pointer to typeInfo         }
        {       [ESP+4] count                   }
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     EBX,EAX
        MOV     ESI,EDX
        MOV     EDI,ECX
        MOV     EBP,[ESP+4+4*4]

        MOV     CL,[EDI]

{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CMP     CL,tkLString
        JE      @@LString
        CMP     CL,tkWString
        JE      @@WString
        CMP     CL,tkUString
        JE      @@UString
        CMP     CL,tkVariant
        JE      @@Variant
        CMP     CL,tkArray
        JE      @@Array
        CMP     CL,tkRecord
        JE      @@Record
        CMP     CL,tkInterface
        JE      @@Interface
        CMP     CL,tkDynArray
        JE      @@DynArray
        MOV     AL,reInvalidPtr
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        JMP     Error

@@LString:
        MOV     EAX,EBX
        MOV     EDX,[ESI]
        CALL    _LStrAsg
        ADD     EBX,4
        ADD     ESI,4
        DEC     EBP
        JNE     @@LString
        JMP     @@exit

@@WString:
        MOV     EAX,EBX
        MOV     EDX,[ESI]
        CALL    _WStrAsg
        ADD     EBX,4
        ADD     ESI,4
        DEC     EBP
        JNE     @@WString
        JMP     @@exit

@@UString:
        MOV     EAX,EBX
        MOV     EDX,[ESI]
        CALL    _UStrAsg
        ADD     EBX,4
        ADD     ESI,4
        DEC     EBP
        JNE     @@UString
        JMP     @@exit

@@Variant:
        MOV     EAX,EBX
        MOV     EDX,ESI
        CALL    _VarCopy
        ADD     EBX,16
        ADD     ESI,16
        DEC     EBP
        JNE     @@Variant
        JMP     @@exit

@@Array:
        XOR     ECX,ECX
        MOV     CL,[EDI+1]
        LEA     EDI,[EDI+ECX+2]
@@ArrayLoop:
        MOV     EAX,EBX
        MOV     EDX,ESI
        MOV     ECX,[EDI+8]
        MOV     ECX,[ECX]
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4 // Negitive Adjustment
{$ENDIF ALIGN_STACK}
        PUSH    dword ptr [EDI+4]
        CALL    _CopyArray
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        ADD     EBX,[EDI]
        ADD     ESI,[EDI]
        DEC     EBP
        JNE     @@ArrayLoop
        JMP     @@exit

@@Record:
        MOV     EAX,EBX
        MOV     EDX,ESI
        MOV     ECX,EDI
        CALL    _CopyRecord
        XOR     EAX,EAX
        MOV     AL,[EDI+1]
        ADD     EBX,[EDI+EAX+2]
        ADD     ESI,[EDI+EAX+2]
        DEC     EBP
        JNE     @@Record
        JMP     @@exit

@@Interface:
        MOV     EAX,EBX
        MOV     EDX,[ESI]
        CALL    _IntfCopy
        ADD     EBX,4
        ADD     ESI,4
        DEC     EBP
        JNE     @@Interface
        JMP     @@exit

@@DynArray:
        MOV     EAX,EBX
        MOV     EDX,[ESI]
        MOV     ECX,EDI
        CALL    _DynArrayAsg
        ADD     EBX,4
        ADD     ESI,4
        DEC     EBP
        JNE     @@DynArray

@@exit:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
        RET     4
end;
{$IFEND}




function _New(size: Longint; typeInfo: Pointer): Pointer;
{$IFDEF PUREPASCAL}
begin
  GetMem(Result, size);
  if Result <> nil then
    _Initialize(Result, typeInfo);
end;
{$ELSE}
asm
        { ->    EAX size of object to allocate  }
        {       EDX pointer to typeInfo         }
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EDX
        CALL    _GetMem
        POP     EDX
        TEST    EAX,EAX
        JE      @@exit
        PUSH    EAX
        CALL    _Initialize
        POP     EAX
@@exit:
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
end;
{$ENDIF !PUREPASCAL}




procedure _Dispose(p: Pointer; typeInfo: Pointer);
{$IFDEF PUREPASCAL}
begin
  _Finalize(p, typeinfo);
  FreeMem(p);
end;
{$ELSE}
asm
        { ->    EAX     Pointer to object to be disposed        }
        {       EDX     Pointer to type info                    }

{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        CALL    _Finalize
        POP     EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        CALL    _FreeMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
end;
{$ENDIF !PUREPASCAL}

{ ----------------------------------------------------- }
{       Wide character support                          }
{ ----------------------------------------------------- }

function WideCharToString(Source: PWideChar): UnicodeString;
begin
  WideCharToStrVar(Source, Result);
end;

function WideCharLenToString(Source: PWideChar; SourceLen: Integer): UnicodeString;
begin
  WideCharLenToStrVar(Source, SourceLen, Result);
end;

procedure WideCharToStrVar(Source: PWideChar; var Dest: UnicodeString);
begin
  _UStrFromPWChar(Dest, Source);
end;

procedure WideCharLenToStrVar(Source: PWideChar; SourceLen: Integer;
  var Dest: UnicodeString);
begin
  _UStrFromPWCharLen(Dest, Source, SourceLen);
end;

procedure WideCharLenToStrVar(Source: PWideChar; SourceLen: Integer;
  var Dest: AnsiString);
begin
  _LStrFromPWCharLen(Dest, Source, SourceLen, DefaultSystemCodePage);
end;

function StringToWideChar(const Source: UnicodeString; Dest: PWideChar;
  DestSize: Integer): PWideChar;
begin
  //Check to see if enough storage is allocated
  if Length(Source) + 1 > DestSize then
  begin
    if DestSize > 0 then
    begin
      Dest[0] := #0;
      Result := Dest;
    end
    else
    begin
      Result := '';
    end;
    Exit;
  end;

  Move(Source[1], Dest[0], Length(Source) * SizeOf(WideChar));
  Dest[Length(Source)] := #0;
  Result := Dest;
end;

{ ----------------------------------------------------- }
{       OLE string support                              }
{ ----------------------------------------------------- }

function OleStrToString(Source: PWideChar): UnicodeString;
begin
  OleStrToStrVar(Source, Result);
end;

procedure OleStrToStrVar(Source: PWideChar; var Dest: AnsiString);
begin
  WideCharLenToStrVar(Source, Length(WideString(Pointer(Source))), Dest);
end;

procedure OleStrToStrVar(Source: PWideChar; var Dest: UnicodeString);
begin
  WideCharLenToStrVar(Source, Length(WideString(Pointer(Source))), Dest);
end;

function StringToOleStr(const Source: AnsiString): PWideChar;
begin
  Result := nil;
  _WStrFromPCharLen(WideString(Pointer(Result)), PAnsiChar(Pointer(Source)), Length(Source));
end;

function StringToOleStr(const Source: UnicodeString): PWideChar; overload;
begin
  Result := nil;
  _WStrFromPWCharLen(WideString(Pointer(Result)), PWideChar(Pointer(Source)), Length(Source));
end;

{ ----------------------------------------------------- }
{       Variant manager support   (obsolete)            }
{ ----------------------------------------------------- }

procedure GetVariantManager(var VarMgr: TVariantManager);
begin
  FillChar(VarMgr, sizeof(VarMgr), 0);
end;

procedure SetVariantManager(const VarMgr: TVariantManager);
begin
end;

function IsVariantManagerSet: Boolean;
begin
  Result := False;
end;





{$IF not defined(CPU386)}
procedure _IntfDispCall(Result: Pointer; const Dispatch: IDispatch;
  DispDesc: PDispDesc; Params: Pointer); cdecl;
type
  TDispCallByIDProc = procedure(Result: Pointer; const Dispatch: IDispatch;
    DispDesc: PDispDesc; Params: Pointer); cdecl;
begin
  TDispCallByIDProc(DispCallByIDProc)(Result, Dispatch, DispDesc, @Params);
end;
{$ELSE}
procedure _IntfDispCall;
asm
{$IFDEF PIC}
        PUSH    EAX
        PUSH    ECX
        CALL    GetGOT
        POP     ECX
        LEA     EAX,[EAX].OFFSET DispCallByIDProc
        MOV     EAX,[EAX]
        XCHG    EAX,[ESP]
        RET
{$ELSE}
        JMP     DispCallByIDProc
{$ENDIF}
end;
{$IFEND}




{$IF not defined(CPU386)}
procedure _DispCallByIDError(Result: Pointer; const Dispatch: IDispatch;
  DispDesc: PDispDesc; Params: Pointer); cdecl;
begin
  Error(reVarDispatch);
end;
{$ELSE}
procedure _DispCallByIDError;
asm
        MOV     AL,reVarDispatch
        JMP     Error
end;
{$IFEND}




{$IF not defined(CPU386)}
procedure _IntfVarCall(Dest: PVarData; const Source: TVarData;
  CallDesc: PCallDesc; Params: Pointer);
{$ELSE}
procedure _IntfVarCall;
{$IFEND}
begin
end;



// 64 bit integer helper routines
//
// These functions always return the 64-bit result in EAX:EDX

// ------------------------------------------------------------------------------
//  64-bit signed multiply
// ------------------------------------------------------------------------------
//
//  Param 1(EAX:EDX), Param 2([ESP+8]:[ESP+4])  ; before reg pushing
//
{$IF defined(CPU386)}
procedure __llmul;
asm //StackAlignSafe
        PUSH  EDX
        PUSH  EAX

  // Param2 : [ESP+16]:[ESP+12]  (hi:lo)
  // Param1 : [ESP+4]:[ESP]      (hi:lo)

        MOV   EAX, [ESP+16]
        MUL   DWORD PTR [ESP]
        MOV   ECX, EAX

        MOV   EAX, [ESP+4]
        MUL   DWORD PTR [ESP+12]
        ADD   ECX, EAX

        MOV   EAX, [ESP]
        MUL   DWORD PTR [ESP+12]
        ADD   EDX, ECX

        POP   ECX
        POP   ECX

        RET   8
end;
{$IFEND}



// ------------------------------------------------------------------------------
//  64-bit signed multiply, with overflow check (98.05.15: overflow not supported yet)
// ------------------------------------------------------------------------------
//
//  Param1 ~= U   (Uh, Ul)
//  Param2 ~= V   (Vh, Vl)
//
//  Param 1(EAX:EDX), Param 2([ESP+8]:[ESP+4])  ; before reg pushing
//
//  compiler-helper function
//  O-flag set on exit   => result is invalid
//  O-flag clear on exit => result is valid
{$IF defined(CPU386)}
procedure __llmulo;
asm //StackAlignSafe
        PUSH   EDX
        PUSH   EAX

        // Param2 : [ESP+16]:[ESP+12]  (hi:lo)
        // Param1 : [ESP+4]:[ESP]      (hi:lo)

        MOV    EAX, [ESP+16]
        MUL    DWORD PTR [ESP]
        MOV    ECX, EAX

        MOV    EAX, [ESP+4]
        MUL    DWORD PTR [ESP+12]
        ADD    ECX, EAX

        MOV    EAX, [ESP]
        MUL    DWORD PTR [ESP+12]
        ADD    EDX, ECX

        POP    ECX
        POP    ECX

        RET    8
end;
{$IFEND}



(* ***** BEGIN LICENSE BLOCK *****
 *
 * The function __lldiv is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): AMD, John O'Harrow and Dennis Christensen
 *
 * ***** END LICENSE BLOCK ***** *)

// ------------------------------------------------------------------------------
//  64-bit signed division
// ------------------------------------------------------------------------------

//
//  Dividend = Numerator, Divisor = Denominator
//
//  Dividend(EAX:EDX), Divisor([ESP+8]:[ESP+4])  ; before reg pushing
//
//
{$IF defined(CPU386)}
procedure __lldiv; //JOH Version
asm //StackAlignSafe
{$IFDEF PC_MAPPED_EXCEPTIONS}
        PUSH    EBP
        MOV     EBP, ESP
{$ENDIF}
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
{$IFDEF PC_MAPPED_EXCEPTIONS}
        MOV     EBX, [ESP+20]
        MOV     ECX, [ESP+24]
{$ELSE !PC_MAPPED_EXCEPTIONS}
        MOV     EBX, [ESP+16]
        MOV     ECX, [ESP+20]
{$ENDIF !PC_MAPPED_EXCEPTIONS}
        MOV     ESI, EDX
        MOV     EDI, ECX
        SAR     ESI, 31
        XOR     EAX, ESI
        XOR     EDX, ESI
        SUB     EAX, ESI
        SBB     EDX, ESI          // EDX:EAX := abs(Dividend)
        SAR     EDI, 31
        XOR     ESI, EDI          // 0 if X and Y have same sign
        XOR     EBX, EDI
        XOR     ECX, EDI
        SUB     EBX, EDI
        SBB     ECX, EDI          // ECX:EBX := abs(Divisor)
        JNZ     @@BigDivisor      // divisor > 32^32-1
        CMP     EDX, EBX          // only one division needed ? (ecx = 0)
        JB      @@OneDiv          // yes, one division sufficient
        MOV     ECX, EAX          // save dividend-lo in ecx
        MOV     EAX, EDX          // get dividend-hi
        XOR     EDX, EDX          // zero extend it into edx:eax
        DIV     EBX               // quotient-hi in eax
        XCHG    EAX, ECX          // ecx = quotient-hi, eax =dividend-lo
@@OneDiv:
        DIV     EBX               // eax = quotient-lo
        MOV     EDX, ECX          // edx = quotient-hi(quotient in edx:eax)
        JMP     @SetSign
@@BigDivisor:
        SUB     ESP, 12           // Create three local variables.
        MOV     [ESP  ], EAX      // dividend_lo
        MOV     [ESP+4], EBX      // divisor_lo
        MOV     [ESP+8], EDX      // dividend_hi
        MOV     EDI, ECX          //  edi:ebx and ecx:esi
        SHR     EDX, 1            // shift both
        RCR     EAX, 1            //  divisor and
        ROR     EDI, 1            //   and dividend
        RCR     EBX, 1            //    right by 1 bit
        BSR     ECX, ECX          // ecx = number of remaining shifts
        SHRD    EBX, EDI, CL      // scale down divisor and
        SHRD    EAX, EDX, CL      //   dividend such that divisor
        SHR     EDX, CL           //    less than 2^32 (i.e. fits in ebx)
        ROL     EDI, 1            // restore original divisor (edi:esi)
        DIV     EBX               // compute quotient
        MOV     EBX, [ESP]        // dividend_lo
        MOV     ECX, EAX          // save quotient
        IMUL    EDI, EAX          // quotient * divisor hi-word (low only)
        MUL     DWORD PTR [ESP+4] // quotient * divisor low word
        ADD     EDX, EDI          // edx:eax = quotient * divisor
        SUB     EBX, EAX          // dividend-lo - (quot.*divisor)-lo
        MOV     EAX, ECX          // get quotient
        MOV     ECX, [ESP+8]      // dividend_hi
        SBB     ECX, EDX          // subtract divisor * quot. from dividend
        SBB     EAX, 0            // Adjust quotient if remainder is negative.
        XOR     EDX, EDX          // clear hi-word of quot (eax<=FFFFFFFFh)
        ADD     ESP, 12           // Remove local variables.
@SetSign:
        XOR     EAX, ESI          // If (quotient < 0),
        XOR     EDX, ESI          //   compute 1's complement of result.
        SUB     EAX, ESI          // If (quotient < 0),
        SBB     EDX, ESI          //   compute 2's complement of result.
@Done:
        POP     EDI
        POP     ESI
        POP     EBX
{$IFDEF PC_MAPPED_EXCEPTIONS}
        POP     EBP
{$ENDIF}
        RET     8
end;
{$IFEND}

// ------------------------------------------------------------------------------
//  64-bit signed division with overflow check (98.05.15: not implementated yet)
// ------------------------------------------------------------------------------

//
//  Dividend = Numerator, Divisor = Denominator
//
//  Dividend(EAX:EDX), Divisor([ESP+8]:[ESP+4])
//  Param 1 (EAX:EDX), Param 2([ESP+8]:[ESP+4])
//
//  Param1 ~= U   (Uh, Ul)
//  Param2 ~= V   (Vh, Vl)
//
//  compiler-helper function
//  O-flag set on exit   => result is invalid
//  O-flag clear on exit => result is valid
//


{$IF defined(CPU386)}
procedure __lldivo;
asm //StackAligned
    //Don't need to stack align only calls local __lldiv which doesn't not call anything else
  // check for overflow condition: min(int64) DIV -1
        push  esi
        mov esi, [esp+12]   // Vh
        and esi, [esp+8]    // Vl
        cmp esi, 0ffffffffh   // V = -1?
        jne @@divok

        mov esi, eax
        or  esi, edx
        cmp esi, 80000000H    // U = min(int64)?
        jne @@divok

@@divOvl:
        mov eax, esi
        pop esi
        dec eax                     // turn on O-flag
        ret 8

@@divok:
        pop esi
        push  dword ptr [esp+8]   // Vh
        push  dword ptr [esp+8]   // Vl (offset is changed from push)

        call  __lldiv
        and eax, eax    // turn off O-flag
        ret 8
end;
{$IFEND}

// ------------------------------------------------------------------------------
//  64-bit unsigned division
// ------------------------------------------------------------------------------

{$IF defined(CPU386)}
//  Dividend(EAX(hi):EDX(lo)), Divisor([ESP+8](hi):[ESP+4](lo))  // before reg pushing

procedure __lludiv;
asm //StackAlignSafe
        PUSH    EBP
{$IFDEF PC_MAPPED_EXCEPTIONS}
        MOV     EBP, ESP
{$ENDIF}
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
//
//       Now the stack looks something like this:
//
//               24[esp]: divisor (high dword)
//               20[esp]: divisor (low dword)
//               16[esp]: return EIP
//               12[esp]: previous EBP
//                8[esp]: previous EBX
//                4[esp]: previous ESI
//                 [esp]: previous EDI
//

//       dividend is pushed last, therefore the first in the args
//       divisor next.
//
        MOV     EBX,20[ESP]             // get the first low word
        MOV     ECX,24[ESP]             // get the first high word

        OR      ECX,ECX
        JNZ     @__lludiv@slow_ldiv     // both high words are zero

        OR      EDX,EDX
        JZ      @__lludiv@quick_ldiv

        or      ebx,ebx
        JZ      @__lludiv@quick_ldiv    // if ecx:ebx == 0 force a zero divide
          // we don't expect this to actually
          // work

@__lludiv@slow_ldiv:
        MOV     EBP,ECX
        MOV     ECX,64                  // shift counter
        XOR     EDI,EDI                 // fake a 64 bit dividend
        XOR     ESI,ESI

@__lludiv@xloop:
        SHL     EAX,1                   // shift dividend left one bit
        RCL     EDX,1
        RCL     ESI,1
        RCL     EDI,1
        CMP     EDI,EBP                 // dividend larger?
        JB      @__lludiv@nosub
        JA      @__lludiv@subtract
        CMP     ESI,EBX                 // maybe
        JB      @__lludiv@nosub

@__lludiv@subtract:
        SUB     ESI,EBX
        SBB     EDI,EBP                 // subtract the divisor
        INC     EAX                     // build quotient

@__lludiv@nosub:
        loop    @__lludiv@xloop
//
//       When done with the loop the four registers values' look like:
//
//       |     edi    |    esi     |    edx     |    eax     |
//       |        remainder        |         quotient        |
//

@__lludiv@finish:
        POP     EDI
        POP     ESI
        POP     EBX
        POP     EBP
        RET     8

@__lludiv@quick_ldiv:
        DIV     EBX                     // unsigned divide
        XOR     EDX,EDX
        JMP     @__lludiv@finish
end;
{$IFEND}

// ------------------------------------------------------------------------------
//  64-bit modulo
// ------------------------------------------------------------------------------

{$IF defined(CPU386)}
//  Dividend(EAX:EDX), Divisor([ESP+8]:[ESP+4])  // before reg pushing

procedure __llmod;
asm //StackAlignSafe
        PUSH    EBP
{$IFDEF PC_MAPPED_EXCEPTIONS}
        MOV     EBP, ESP
{$ENDIF}
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        XOR     EDI,EDI
//
//       dividend is pushed last, therefore the first in the args
//       divisor next.
//
        MOV     EBX,20[ESP]             // get the first low word
        MOV     ECX,24[ESP]             // get the first high word
        OR      ECX,ECX
        JNZ     @__llmod@slow_ldiv      // both high words are zero

        OR      EDX,EDX
        JZ      @__llmod@quick_ldiv

        OR      EBX,EBX
        JZ      @__llmod@quick_ldiv     // if ecx:ebx == 0 force a zero divide
          // we don't expect this to actually
          // work
@__llmod@slow_ldiv:
//
//               Signed division should be done.  Convert negative
//               values to positive and do an unsigned division.
//               Store the sign value in the next higher bit of
//               di (test mask of 4).  Thus when we are done, testing
//               that bit will determine the sign of the result.
//
        OR      EDX,EDX                 // test sign of dividend
        JNS     @__llmod@onepos
        NEG     EDX
        NEG     EAX
        SBB     EDX,0                   // negate dividend
        OR      EDI,1

@__llmod@onepos:
        OR      ECX,ECX                 // test sign of divisor
        JNS     @__llmod@positive
        NEG     ECX
        NEG     EBX
        SBB     ECX,0                   // negate divisor

@__llmod@positive:
        MOV     EBP,ECX
        MOV     ECX,64                  // shift counter
        PUSH    EDI                     // save the flags
//
//       Now the stack looks something like this:
//
//               24[esp]: divisor (high dword)
//               20[esp]: divisor (low dword)
//               16[esp]: return EIP
//               12[esp]: previous EBP
//                8[esp]: previous EBX
//                4[esp]: previous ESI
//                 [esp]: previous EDI
//
        XOR     EDI,EDI                 // fake a 64 bit dividend
        XOR     ESI,ESI

@__llmod@xloop:
        SHL     EAX,1                   // shift dividend left one bit
        RCL     EDX,1
        RCL     ESI,1
        RCL     EDI,1
        CMP     EDI,EBP                 // dividend larger?
        JB      @__llmod@nosub
        JA      @__llmod@subtract
        CMP     ESI,EBX                 // maybe
        JB      @__llmod@nosub

@__llmod@subtract:
        SUB     ESI,EBX
        SBB     EDI,EBP                 // subtract the divisor
        INC     EAX                     // build quotient

@__llmod@nosub:
        LOOP    @__llmod@xloop
//
//       When done with the loop the four registers values' look like:
//
//       |     edi    |    esi     |    edx     |    eax     |
//       |        remainder        |         quotient        |
//
        MOV     EAX,ESI
        mov     edx,edi                 // use remainder

        POP     EBX                     // get control bits
        TEST    EBX,1                   // needs negative
        JZ      @__llmod@finish
        NEG     EDX
        NEG     EAX
        SBB     EDX,0                    // negate

@__llmod@finish:
        POP     EDI
        POP     ESI
        POP     EBX
        POP     EBP
        RET     8

@__llmod@quick_ldiv:
        DIV     EBX                     // unsigned divide
        XCHG    EAX,EDX
        XOR     EDX,EDX
        JMP     @__llmod@finish
end;
{$IFEND}

// ------------------------------------------------------------------------------
//  64-bit signed modulo with overflow (98.05.15: overflow not yet supported)
// ------------------------------------------------------------------------------

//  Dividend(EAX:EDX), Divisor([ESP+8]:[ESP+4])
//  Param 1 (EAX:EDX), Param 2([ESP+8]:[ESP+4])
//
//  Param1 ~= U   (Uh, Ul)
//  Param2 ~= V   (Vh, Vl)
//
//  compiler-helper function
//  O-flag set on exit   => result is invalid
//  O-flag clear on exit => result is valid
//


{$IF defined(CPU386)}
procedure __llmodo;
asm //StackAlignSafe
    //Don't need to stack align only calls local __llmod which doesn't not call anything else
  // check for overflow condition: min(int64) MOD -1
        PUSH  ESI
        MOV   ESI, [ESP+12]     // Vh
        AND   ESI, [ESP+8]      // Vl
        CMP   ESI, 0FFFFFFFFH   // V = -1?
        JNE   @@modok

        MOV   ESI, EAX
        OR    ESI, EDX
        CMP   ESI, 80000000H    // U = min(int64)?
        JNE   @@modok

@@modOvl:
        MOV   EAX, ESI
        POP   ESI
        DEC   EAX               // turn on O-flag
        RET   8

@@modok:
        POP   ESI
        PUSH  DWORD PTR [ESP+8] // Vh
        PUSH  DWORD PTR [ESP+8] // Vl (offset is changed from push)

        CALL  __llmod
        AND   EAX, EAX    // turn off O-flag
        RET   8
end;
{$IFEND}

// ------------------------------------------------------------------------------
//  64-bit unsigned modulo
// ------------------------------------------------------------------------------
//  Dividend(EAX(hi):EDX(lo)), Divisor([ESP+8](hi):[ESP+4](lo))  // before reg pushing


{$IF defined(CPU386)}
procedure __llumod;
asm //StackAlignSafe
        PUSH    EBP
{$IFDEF PC_MAPPED_EXCEPTIONS}
        MOV     EBP, ESP
{$ENDIF}
        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
//
//       Now the stack looks something like this:
//
//               24[esp]: divisor (high dword)
//               20[esp]: divisor (low dword)
//               16[esp]: return EIP
//               12[esp]: previous EBP
//                8[esp]: previous EBX
//                4[esp]: previous ESI
//                 [esp]: previous EDI
//

//       dividend is pushed last, therefore the first in the args
//       divisor next.
//
        MOV     EBX,20[ESP]             // get the first low word
        MOV     ECX,24[ESP]             // get the first high word
        OR      ECX,ECX
        JNZ     @__llumod@slow_ldiv     // both high words are zero

        OR      EDX,EDX
        JZ      @__llumod@quick_ldiv

        OR      EBX,EBX
        JZ      @__llumod@quick_ldiv    // if ecx:ebx == 0 force a zero divide
          // we don't expect this to actually
          // work
@__llumod@slow_ldiv:
        MOV     EBP,ECX
        MOV     ECX,64                  // shift counter
        XOR     EDI,EDI                 // fake a 64 bit dividend
        XOR     ESI,ESI                 //

@__llumod@xloop:
        SHL     EAX,1                   // shift dividend left one bit
        RCL     EDX,1
        RCL     ESI,1
        RCL     EDI,1
        CMP     EDI,EBP                 // dividend larger?
        JB      @__llumod@nosub
        JA      @__llumod@subtract
        CMP     ESI,EBX                 // maybe
        JB      @__llumod@nosub

@__llumod@subtract:
        SUB     ESI,EBX
        SBB     EDI,EBP                 // subtract the divisor
        INC     EAX                     // build quotient

@__llumod@nosub:
        LOOP    @__llumod@xloop
//
//       When done with the loop the four registers values' look like:
//
//       |     edi    |    esi     |    edx     |    eax     |
//       |        remainder        |         quotient        |
//
        MOV     EAX,ESI
        MOV     EDX,EDI                 // use remainder

@__llumod@finish:
        POP     EDI
        POP     ESI
        POP     EBX
        POP     EBP
        RET     8

@__llumod@quick_ldiv:
        DIV     EBX                     // unsigned divide
        XCHG    EAX,EDX
        XOR     EDX,EDX
        jmp     @__llumod@finish
end;
{$IFEND}

// ------------------------------------------------------------------------------
//  64-bit shift left
// ------------------------------------------------------------------------------

{$IF defined(CPU386)}
//
// target (EAX:EDX) count (ECX)
//


procedure __llshl;
asm //StackAlignSafe
        AND   CL, $3F
        CMP   CL, 32
        JL    @__llshl@below32
        MOV   EDX, EAX
        SHL   EDX, CL
        XOR   EAX, EAX
        RET

@__llshl@below32:
        SHLD  EDX, EAX, CL
        SHL   EAX, CL
        RET
end;
{$IFEND}

// ------------------------------------------------------------------------------
//  64-bit signed shift right
// ------------------------------------------------------------------------------
// target (EAX:EDX) count (ECX)


{$IF defined(CPU386)}
procedure __llshr;
asm //StackAlignSafe
        AND   CL, $3F
        CMP   CL, 32
        JL    @__llshr@below32
        MOV   EAX, EDX
        CDQ
        SAR   EAX,CL
        RET

@__llshr@below32:
        SHRD  EAX, EDX, CL
        SAR   EDX, CL
        RET
end;
{$IFEND}

// ------------------------------------------------------------------------------
//  64-bit unsigned shift right
// ------------------------------------------------------------------------------

{$IF defined(CPU386)}
// target (EAX:EDX) count (ECX)


procedure __llushr;
asm //StackAlignSafe
        and cl, $3F
        cmp cl, 32
        jl  @__llushr@below32
        mov eax, edx
        xor edx, edx
        shr eax, cl
        ret

@__llushr@below32:
        shrd  eax, edx, cl
        shr edx, cl
        ret
end;
{$IFEND}

function _StrUInt64Digits(val: UInt64; width: Integer; sign: Boolean): ShortString;
var
  d: array[0..31] of Char;  { need 19 digits and a sign }
  i, k: Integer;
  spaces: Integer;
begin
  { Produce an ASCII representation of the number in reverse order }
  i := 0;
  repeat
    d[i] := Chr( (val mod 10) + Ord('0') );
    Inc(i);
    val := val div 10;
  until val = 0;
  if sign then
  begin
    d[i] := '-';
    Inc(i);
  end;

  { Fill the Result with the appropriate number of blanks }
  if width > 255 then
    width := 255;
  k := 1;
  spaces := width - i;
  while k <= spaces do
  begin
    Result[k] := AnsiChar(' ');
    Inc(k);
  end;

  { Fill the Result with the number }
  while i > 0 do
  begin
    Dec(i);
    Result[k] := AnsiChar(d[i]);
    Inc(k);
  end;

  { Result is k-1 characters long }
  SetLength(Result, k-1);
end;

function _StrInt64(val: Int64; width: Integer): ShortString;
begin
  Result := _StrUInt64Digits(Abs(val), width, val < 0);
end;

function _Str0Int64(val: Int64): ShortString;
begin
  Result := _StrInt64(val, 0);
end;

function _StrUInt64(val: UInt64; width: Integer): ShortString;
begin
  Result := _StrUInt64Digits(val, width, False);
end;

function _Str0UInt64(val: Int64): ShortString;
begin
  Result := _StrUInt64(val, 0);
end;




{$IF not defined(CPU386)}
function _WriteInt64(var t: TTextRec; val: Int64; width: LongInt): Pointer;
var
  s: ShortString;
begin
  s := _StrInt64(val, 0);
  Result := _WriteString(t, s, width);
end;
{$ELSE}
procedure       _WriteInt64;
asm
{       PROCEDURE _WriteInt64( VAR t: Text; val: Int64; with: Longint);        }
{     ->EAX     Pointer to file record  }
{       [ESP+4] Value                   }
{       EDX     Field width             }
{$IFDEF ALIGN_STACK}
        SUB     ESP,12
{$ENDIF ALIGN_STACK}
        SUB     ESP,32          { VAR s: String[31];    }

        PUSH    EAX
        PUSH    EDX

{$IFDEF ALIGN_STACK}
        PUSH    dword ptr [ESP+12+8+32+8]    { Str( val : 0, s );    }
        PUSH    dword ptr [ESP+12+8+32+8]
{$ELSE !ALIGN_STACK}
        PUSH    dword ptr [ESP+8+32+8]    { Str( val : 0, s );    }
        PUSH    dword ptr [ESP+8+32+8]
{$ENDIF ALIGN_STACK}
        XOR     EAX,EAX
        LEA     EDX,[ESP+8+8]
        CALL    _StrInt64

        POP     ECX
        POP     EAX
        MOV     EDX,ESP         { Write( t, s : width );}
        CALL    _WriteString
        ADD     ESP,32
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        RET     8
end;
{$IFEND}




{$IF not defined(CPU386)}
function _Write0Int64(var t: TTextRec; val: Int64): Pointer;
begin
  Result := _WriteInt64(t, val, 0);
end;
{$ELSE}
procedure       _Write0Int64;
asm
{       PROCEDURE _Write0Int64( VAR t: Text; val: Int64);       }
{     ->EAX     Pointer to file record  }
{       EDX     Value                   }
        XOR     EDX,EDX
        JMP     _WriteInt64
end;
{$IFEND}




{$IF not defined(CPU386)}
function _WriteUInt64(var t: TTextRec; val: UInt64; width: LongInt): Pointer;
var
  s: ShortString;
begin
  s := _StrUInt64Digits(val, 0, False);
  Result := _WriteString(t, s, width);
end;
{$ELSE}
procedure       _WriteUInt64;
asm
{       PROCEDURE _WriteUInt64( VAR t: Text; val: UInt64; width: LongInt);      }
{     ->EAX     Pointer to file record  }
{       [ESP+4] Value                   }
{       EDX     Field width             }
{$IFDEF ALIGN_STACK}
        SUB     ESP,12
{$ENDIF ALIGN_STACK}
        SUB     ESP,32          { VAR s: String[31];    }

        PUSH    EAX
        PUSH    EDX

{$IFDEF ALIGN_STACK}
        PUSH    dword ptr [ESP+12+8+32+8]    { Str( val : 0, s );    }
        PUSH    dword ptr [ESP+12+8+32+8]
{$ELSE !ALIGN_STACK}
        PUSH    dword ptr [ESP+8+32+8]    { Str( val : 0, s );    }
        PUSH    dword ptr [ESP+8+32+8]
{$ENDIF ALIGN_STACK}
        XOR     EAX,EAX
        LEA     EDX,[ESP+8+8]
        CALL    _StrUInt64

        POP     ECX
        POP     EAX

        MOV     EDX,ESP         { Write( t, s : width );}
        CALL    _WriteString

        ADD     ESP,32
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        RET     8
end;
{$IFEND}




{$IF not defined(CPU386)}
function _Write0UInt64(var t: TTextRec; val: UInt64): Pointer;
begin
  Result := _WriteUInt64(t, val, 0);
end;
{$ELSE}
procedure       _Write0UInt64;
asm
{       PROCEDURE _Write0UInt64( VAR t: Text; val: UInt64);      }
{     ->EAX     Pointer to file record  }
{       EDX     Value                   }
        XOR     EDX,EDX
        JMP     _WriteUInt64
end;
{$IFEND}

function _ValInt64L(const s: AnsiString; var code: Integer): Int64;
begin
  Result := _ValInt64(string(s), code);
end;

{$IF not defined(CPU386)}

function _ReadInt64(var t: TTextRec): Int64;
type
  TStrRec32 = packed record
    hdr: StrRec;
    data: array[0..35] of Byte;
  end;
var
  s: TStrRec32;
  p: PByte;
  len: Integer;
  count: Integer;
  c: Word;
  code: Integer;
begin
  if _SeekEof(t) then
    Result := 0
  else
  begin
    p := @s.data[0];
    for count := 1 to 32 do
    begin
      c := _ReadCharEx(t);
      if (c and $ff) <= $20 then
      begin
        if ((c and $ff00) shr 8) <> cEof then Dec(t.BufPos);
        break;
      end;
      p^ := c;
      Inc(p);
    end;
    p^ := 0;
    s.hdr.codePage := CP_ACP;
    s.hdr.elemSize := 1;
    s.hdr.refCnt := -1;
    s.hdr.length := p - PByte(@s.data[0]);
    Result := _ValInt64L(PAnsiString(@s.data)^, code);
    if code <> 0 then
      SetInOutRes(106);
  end;
end;
{$ELSE}


procedure _ReadInt64;
asm
        // -> EAX Pointer to text record
        // <- EAX:EDX Result

        PUSH  EBX
        PUSH  ESI
        PUSH  EDI
        SUB   ESP,36      // var numbuf: String[32];
        PUSH  0           // String Length
        PUSH  -1          // Refcount (-1 = string constant)
        PUSH  $00010000   // elemSize = 1, codePage = CP_ACP

        MOV   ESI,EAX
        CALL  _SeekEof
        DEC   AL
        JZ    @@eof

        LEA   EDI,[ESP+skew]     // EDI -> numBuf[0]
        MOV   BL,32
@@loop:
        MOV   EAX,ESI
        CALL  _ReadChar
        CMP   AL,' '
        JBE   @@endNum
        STOSB
        DEC   BL
        JNZ   @@loop
@@convert:
        MOV   byte ptr [EDI],0
        LEA   EAX,[ESP+skew]     // EAX -> numBuf
        MOV   ECX,EDI
        SUB   ECX,EAX
        MOV   [EAX-Skew].StrRec.length,ECX
{$IFDEF ALIGN_STACK}
        SUB   ESP, 12
{$ENDIF ALIGN_STACK}
        PUSH  EDX                // allocate code result
        MOV   EDX,ESP            // pass pointer to code

        CALL  _ValInt64L         // convert
        POP   ECX                // pop code result into ECX
        TEST  ECX,ECX
        JZ    @@exit
        MOV   EAX,106
        CALL  SetInOutRes

@@exit:
{$IFDEF ALIGN_STACK}
        ADD   ESP, 12
{$ENDIF ALIGN_STACK}
        ADD   ESP,36 + 4 + 4 + 2 + 2 // length, refCnt, elemSize, codePage
        POP   EDI
        POP   ESI
        POP   EBX
        RET

@@endNum:
        CMP   AH,cEof
        JE    @@convert
        DEC   [ESI].TTextRec.BufPos
        JMP   @@convert

@@eof:
        XOR   EAX,EAX
        JMP   @@exit
end;
{$IFEND}

function _ValInt64(const s: string; var code: Integer): Int64;
var
  i: Integer;
  dig: Integer;
  sign: Boolean;
  empty: Boolean;
begin
  i := 1;
  dig := 0;
  Result := 0;
  if s = '' then
  begin
    code := i;
    exit;
  end;
  while s[i] = Char(' ') do
    Inc(i);
  sign := False;
  if s[i] =  Char('-') then
  begin
    sign := True;
    Inc(i);
  end
  else if s[i] =  Char('+') then
    Inc(i);
  empty := True;
  if (s[i] =  Char('$')) or (Upcase(s[i]) =  Char('X'))
    or ((s[i] =  Char('0')) and (I < Length(S)) and (Upcase(s[i+1]) =  Char('X'))) then
  begin
    if s[i] =  Char('0') then
      Inc(i);
    Inc(i);
    while True do
    begin
      case   Char(s[i]) of
       Char('0').. Char('9'): dig := Ord(s[i]) -  Ord('0');
       Char('A').. Char('F'): dig := Ord(s[i]) - (Ord('A') - 10);
       Char('a').. Char('f'): dig := Ord(s[i]) - (Ord('a') - 10);
      else
        break;
      end;
      if (Result < 0) or (Result > (High(Int64) shr 3)) then
        Break;
      Result := Result shl 4 + dig;
      Inc(i);
      empty := False;
    end;
    if sign then
      Result := - Result;
  end
  else
  begin
    while True do
    begin
      case  Char(s[i]) of
        Char('0').. Char('9'): dig := Ord(s[i]) - Ord('0');
      else
        break;
      end;
      if (Result < 0) or (Result > (High(Int64) div 10)) then
        break;
      Result := Result*10 + dig;
      Inc(i);
      empty := False;
    end;
    if sign then
      Result := - Result;
    if (Result <> 0) and (sign <> (Result < 0)) then
      Dec(i);
  end;
  if (s[i] <> Char(#0)) or empty then
    code := i
  else
    code := 0;
end;


{ ----------------------------------------------------- }
{       Compiler helper for Dynamic array support       }
{ ----------------------------------------------------- }




{$IF not defined(CPU386)}
function _DynArrayLength(const A: Pointer): LongInt;
begin
  Result := 0;
  if A <> nil then
    Result := PDynArrayRec(NativeInt(A) - SizeOf(TDynArrayRec)).Length;
end;
{$ELSE}
procedure _DynArrayLength;
asm
{       FUNCTION _DynArrayLength(const a: array of ...): Longint; }
{     ->EAX     Pointer to array or nil                           }
{     <-EAX     High bound of array + 1 or 0                      }
        TEST    EAX,EAX
        JZ      @@skip
        MOV     EAX,[EAX-4]
@@skip:
end;
{$IFEND}



{$IF not defined(CPU386)}
function _DynArrayHigh(const A: Pointer): LongInt;
begin
  Result := _DynArrayLength(A) - 1;
end;
{$ELSE}
procedure _DynArrayHigh;
asm
{       FUNCTION _DynArrayHigh(const a: array of ...): Longint; }
{     ->EAX     Pointer to array or nil                         }
{     <-EAX     High bound of array or -1                       }
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    _DynArrayLength
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
        DEC     EAX
end;
{$IFEND}



procedure CopyArray(dest, source, typeInfo: Pointer; cnt: Integer);
{$IF not defined(CPU386)}
begin
  _CopyArray(dest, source, typeInfo, cnt);
end;
{$ELSE}
asm //StackAlignSafe
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    dword ptr [EBP+8]
        CALL    _CopyArray
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
end;
{$IFEND}



procedure FinalizeArray(P, TypeInfo: Pointer; Count: Cardinal);
{$IF not defined(CPU386)}
begin
  _FinalizeArray(P, TypeInfo, Count);
end;
{$ELSE}
asm
        JMP     _FinalizeArray
end;
{$IFEND}


procedure DynArrayClear(var a: Pointer; typeInfo: Pointer);
{$IF not defined(CPU386)}
begin
  _DynArrayClear(a, typeInfo);
end;
{$ELSE}
asm //StackAlignSafe
{$IFDEF ALIGN_STACK}
        SUB     ESP, 12
{$ENDIF ALIGN_STACK}
        CALL    _DynArrayClear
{$IFDEF ALIGN_STACK}
        ADD     ESP, 12
{$ENDIF ALIGN_STACK}
end;
{$IFEND}

procedure DynArraySetLength(var a: Pointer; typeInfo: Pointer; dimCnt: Longint; lengthVec: PLongint);
var
  i: Integer;
  newLength, oldLength, minLength: Longint;
  elSize: Longint;
  neededSize: Longint;
  p, pp: Pointer;
begin
  p := a;

  // Fetch the new length of the array in this dimension, and the old length
  newLength := PLongint(lengthVec)^;
  if newLength <= 0 then
  begin
    if newLength < 0 then
      Error(reRangeError);
    DynArrayClear(a, typeInfo);
    exit;
  end;

  oldLength := 0;
  if p <> nil then
  begin
    Dec(NativeInt(p), SizeOf(TDynArrayRec));
    oldLength := PDynArrayRec(p).Length;
  end;

  // Calculate the needed size of the heap object
  Inc(PAnsiChar(typeInfo), Length(PDynArrayTypeInfo(typeInfo).name));
  elSize := PDynArrayTypeInfo(typeInfo).elSize;
  if PDynArrayTypeInfo(typeInfo).elType <> nil then
    typeInfo := PDynArrayTypeInfo(typeInfo).elType^
  else
    typeInfo := nil;
  neededSize := newLength*elSize;
  if neededSize div newLength <> elSize then
    Error(reRangeError);
  Inc(neededSize, SizeOf(TDynArrayRec));
  if neededSize < 0 then
    Error(reRangeError);

  // If the heap object isn't shared (ref count = 1), just resize it. Otherwise, we make a copy
  if (p = nil) or (PDynArrayRec(p).RefCnt = 1) then
  begin
    pp := p;
    if (newLength < oldLength) and (typeInfo <> nil) then
      FinalizeArray(PAnsiChar(p) + SizeOf(TDynArrayRec) + newLength*elSize, typeInfo, oldLength - newLength);
    ReallocMem(pp, neededSize);
    p := pp;
  end
  else
  begin
    Dec(PDynArrayRec(p).RefCnt);
    GetMem(p, neededSize);
    minLength := oldLength;
    if minLength > newLength then
      minLength := newLength;
    if typeInfo <> nil then
    begin
      FillChar((PAnsiChar(p) + SizeOf(TDynArrayRec))^, minLength*elSize, 0);
      CopyArray(PAnsiChar(p) + SizeOf(TDynArrayRec), a, typeInfo, minLength)
    end
    else
      Move(PAnsiChar(a)^, (PAnsiChar(p) + SizeOf(TDynArrayRec))^, minLength*elSize);
  end;

  // The heap object will now have a ref count of 1 and the new length
  PDynArrayRec(p).RefCnt := 1;
  PDynArrayRec(p).Length := newLength;
  Inc(NativeInt(p), SizeOf(TDynArrayRec));

  // Set the new memory to all zero bits
  FillChar((PAnsiChar(p) + elSize * oldLength)^, elSize * (newLength - oldLength), 0);

  // Take care of the inner dimensions, if any
  if dimCnt > 1 then
  begin
    Inc(lengthVec);
    Dec(dimCnt);
    for i := 0 to newLength-1 do
      DynArraySetLength(PPointerArray(p)[i], typeInfo, dimCnt, lengthVec);
  end;
  a := p;
end;




{$IF not defined(CPU386)}
procedure _DynArraySetLength(var A: Pointer; TypeInfo: Pointer; DimCnt: LongInt; LengthVec: LongInt);
begin
  DynArraySetLength(A, TypeInfo, DimCnt, @LengthVec);
end;
{$ELSE}
procedure _DynArraySetLength;
asm
{       PROCEDURE _DynArraySetLength(var a: dynarray; typeInfo: PDynArrayTypeInfo; dimCnt: Longint; lengthVec: ^Longint) }
{     ->EAX     Pointer to dynamic array (= pointer to pointer to heap object) }
{       EDX     Pointer to type info for the dynamic array                     }
{       ECX     number of dimensions                                           }
{       [ESP+4] dimensions                                                     }
{$IFDEF ALIGN_STACK}
        PUSH    EBP        // Setup stack frame in case of exception
        MOV     EBP, ESP   // to prevent unwinder from freaking out
        SUB     ESP, 4
        PUSH    ESP
        ADD     dword ptr [ESP],12
{$ELSE !ALIGN_STACK}
        PUSH    ESP
        ADD     dword ptr [ESP],4
{$ENDIF !ALIGN_STACK}
        CALL    DynArraySetLength
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
        POP     EBP
{$ENDIF ALIGN_STACK}
end;
{$IFEND}

{$IF defined(CPU386)}
procedure _DynArrayCopy(A: Pointer; TypeInfo: Pointer; var Result: Pointer);
begin
  if A <> nil then
    _DynArrayCopyRange(A, TypeInfo, 0, PDynArrayRec(NativeInt(A) - SizeOf(TDynArrayRec)).Length, Result)
  else
    _DynArrayClear(Result, TypeInfo);
end;
{$ELSE}
procedure _DynArrayCopy(var Result: Pointer; A: Pointer; TypeInfo: Pointer);
begin
  if A <> nil then
    _DynArrayCopyRange(Result, A, TypeInfo, 0, PDynArrayRec(NativeInt(A) - SizeOf(TDynArrayRec)).Length)
  else
    _DynArrayClear(Result, TypeInfo);
end;
{$IFEND}

{$IF defined(CPU386)}
procedure _DynArrayCopyRange(A: Pointer; TypeInfo: Pointer; Index, Count : Integer; var Result: Pointer);
{$ELSE}
procedure _DynArrayCopyRange(var Result: Pointer; A: Pointer; TypeInfo: Pointer; Index, Count : Integer);
{$IFEND}
var
  arrayLength: Integer;
  elSize: Integer;
  typeInf: PDynArrayTypeInfo;
  p: Pointer;
begin
  p := nil;
  if A <> nil then
  begin
    typeInf := TypeInfo;

    // Limit index and count to values within the array
    if Index < 0 then
    begin
      Inc(Count, Index);
      Index := 0;
    end;
    arrayLength := PDynArrayRec(NativeInt(A) - SizeOf(TDynArrayRec)).Length;
    if Index > arrayLength then
      Index := arrayLength;
    if Count > arrayLength - Index then
      Count := arrayLength - Index;
    if Count < 0 then
      Count := 0;

    if Count > 0 then
    begin
      // Figure out the size and type descriptor of the element type
      Inc(NativeInt(typeInf), Byte(typeInf.name[0]));
      elSize := typeInf.elSize;
      if typeInf.elType <> nil then
        typeInf := typeInf.elType^
      else
        typeInf := nil;

      // Allocate the amount of memory needed
      GetMem(p, Count * elSize + SizeOf(TDynArrayRec));

      // The reference count of the new array is 1, the length is count
      PDynArrayRec(p).RefCnt := 1;
      PDynArrayRec(p).Length := Count;
      Inc(NativeInt(p), SizeOf(TDynArrayRec));
      Inc(NativeInt(A), Index * elSize);

      // If the element type needs destruction, we must copy each element,
      // otherwise we can just copy the bits
      if Count > 0 then
      begin
        if typeInf <> nil then
        begin
          FillChar(p^, Count * elSize, 0);
          CopyArray(p, A, typeInf, Count)
        end
        else
          Move(A^, p^, Count * elSize);
      end;
    end;
  end;
  DynArrayClear(Result, TypeInfo);
  Result := p;
end;

{$IF not defined(CPU386)}

function _DynArrayClear(var A: Pointer; TypeInfo: Pointer): Pointer;
var
  P: Pointer;
  Len: LongInt;
begin
  // Nothing to do if Pointer to heap object is nil
  P := A;
  if P <> nil then
  begin
    // Set the variable to be finalized to nil
    A := nil;
    // Decrement ref count. Nothing to do if not zero now.
    if InterlockedDecrement(PDynArrayRec(NativeInt(P) - SizeOf(TDynArrayRec)).RefCnt) = 0 then
    begin
      // Fetch the type descriptor of the elements
      Inc(NativeInt(TypeInfo), Byte(PDynArrayTypeInfo(TypeInfo).name[0]));
      if PDynArrayTypeInfo(TypeInfo).elType <> nil then
      begin
        Len := PDynArrayRec(NativeInt(P) - SizeOf(TDynArrayRec)).Length;
        if Len <> 0 then
        begin
          TypeInfo := PDynArrayTypeInfo(TypeInfo).elType^;
          _FinalizeArray(P, TypeInfo, Len);
        end;
      end;
      // Now deallocate the array
      Dec(NativeInt(P), SizeOf(TDynArrayRec));
      _FreeMem(P);
    end;
  end;
  Result := @A;
end;
{$ELSE}

procedure _DynArrayClear;
asm //StackAlignSafe
{     ->EAX     Pointer to dynamic array (Pointer to pointer to heap object)}
{       EDX     Pointer to type info                                        }

        {       Nothing to do if Pointer to heap object is nil }
        MOV     ECX,[EAX]
        TEST    ECX,ECX
        JE      @@exit

        {       Set the variable to be finalized to nil }
        MOV     dword ptr [EAX],0

        {       Decrement ref count. Nothing to do if not zero now. }
   LOCK DEC     dword ptr [ECX-8]
        JNE     @@exit

        {       Save the source - we're supposed to return it }
        PUSH    EAX
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        MOV     EAX,ECX

        {       Fetch the type descriptor of the elements }
        XOR     ECX,ECX
        MOV     CL,[EDX].TDynArrayTypeInfo.name;
        MOV     EDX,[EDX+ECX].TDynArrayTypeInfo.elType;

        {       If it's non-nil, finalize the elements }
        TEST    EDX,EDX
        JE      @@noFinalize
        MOV     ECX,[EAX-4]
        TEST    ECX,ECX
        JE      @@noFinalize
        MOV     EDX,[EDX]
        {       Save/restore the array around the call to _FinalizeArray }
{$IFDEF ALIGN_STACK}
        MOV     [ESP], EAX
{$ELSE !ALIGN_STACK}
        PUSH    EAX
{$ENDIF !ALIGN_STACK}
        CALL    _FinalizeArray
{$IFDEF ALIGN_STACK}
        MOV     EAX, [ESP]
{$ELSE !ALIGN_STACK}
        POP     EAX
{$ENDIF !ALIGN_STACK}
@@noFinalize:
        {       Now deallocate the array }
        SUB     EAX,8
        CALL    _FreeMem
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        POP     EAX
@@exit:
end;
{$IFEND}



{$IF not defined(CPU386)}
procedure _DynArrayAsg(var Dest: Pointer; Src: Pointer; TypeInfo: Pointer);
begin
  _DynArrayAddRef(Src);
  _DynArrayClear(Dest, TypeInfo);
  Dest := Src;
end;
{$ELSE}
procedure _DynArrayAsg;
asm
{     ->EAX     Pointer to destination (pointer to pointer to heap object) }
{       EDX     source (pointer to heap object) }
{       ECX     Pointer to rtti describing dynamic array }

        PUSH    EBX
        MOV     EBX,[EAX]

        {       Increment ref count of source if non-nil }

        TEST    EDX,EDX
        JE      @@skipInc
   LOCK INC     dword ptr [EDX-8]
@@skipInc:
        {       Dec ref count of destination - if it becomes 0, clear dest }
        TEST    EBX,EBX
        JE      @@skipClear
   LOCK DEC     dword ptr[EBX-8]
        JNZ     @@skipClear
        PUSH    EAX
        PUSH    EDX
        MOV     EDX,ECX
        INC     dword ptr[EBX-8]
        {       Stack is aligned at this point }
        CALL    _DynArrayClear
        POP     EDX
        POP     EAX
@@skipClear:
        {       Finally store source into destination }
        MOV     [EAX],EDX

        POP     EBX
end;
{$IFEND}




{$IF not defined(CPU386)}
procedure _DynArrayAddRef(P: Pointer);
begin
  if P <> nil then
    InterlockedIncrement(PDynArrayRec(NativeInt(P) - SizeOf(TDynArrayRec)).RefCnt);
end;
{$ELSE}
procedure _DynArrayAddRef;
asm
{     ->EAX     Pointer to heap object }
        TEST    EAX,EAX
        JE      @@exit
   LOCK INC     dword ptr [EAX-8]
@@exit:
end;
{$IFEND}





{$IF not defined(CPU386)}
function DynArrayIndex(const P: Pointer; const Indices: array of Integer; const TypInfo: Pointer): Pointer;
begin
end;
{$ELSE}
function DynArrayIndex(const P: Pointer; const Indices: array of Integer; const TypInfo: Pointer): Pointer;
asm
        {     ->EAX     P                       }
        {       EDX     Pointer to Indices      }
        {       ECX     High bound of Indices   }
        {       [EBP+8] TypInfo                 }

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI
        PUSH    EBP

        MOV     ESI,EDX
        MOV     EDI,[EBP+8]
        MOV     EBP,EAX

        XOR     EBX,EBX                 {  for i := 0 to High(Indices) do       }
        TEST    ECX,ECX
        JGE     @@start
@@loop:
        MOV     EBP,[EBP]
@@start:
        XOR     EAX,EAX
        MOV     AL,[EDI].TDynArrayTypeInfo.name
        ADD     EDI,EAX
        MOV     EAX,[ESI+EBX*4]         {    P := P + Indices[i]*TypInfo.elSize }
        MUL     [EDI].TDynArrayTypeInfo.elSize
        MOV     EDI,[EDI].TDynArrayTypeInfo.elType
        TEST    EDI,EDI
        JE      @@skip
        MOV     EDI,[EDI]
@@skip:
        ADD     EBP,EAX
        INC     EBX
        CMP     EBX,ECX
        JLE     @@loop

@@loopEnd:

        MOV     EAX,EBP

        POP     EBP
        POP     EDI
        POP     ESI
        POP     EBX
end;
{$IFEND}


{ Returns the DynArrayTypeInfo of the Element Type of the specified DynArrayTypeInfo }
function DynArrayElTypeInfo(typeInfo: PDynArrayTypeInfo): PDynArrayTypeInfo;
begin
  Result := nil;
  if typeInfo <> nil then
  begin
    Inc(PByte(typeInfo), Length(typeInfo.name));
    if typeInfo.elType <> nil then
      Result := typeInfo.elType^;
  end;
end;

{ Returns # of dimemsions of the DynArray described by the specified DynArrayTypeInfo}
function DynArrayDim(typeInfo: PDynArrayTypeInfo): Integer;
begin
  Result := 0;
  while (typeInfo <> nil) and (typeInfo.kind = tkDynArray) do
  begin
    Inc(Result);
    typeInfo := DynArrayElTypeInfo(typeInfo);
  end;
end;




{ Returns size of the Dynamic Array}
function DynArraySize(A: Pointer): Integer;
{$IF not defined(CPU386)}
begin
  Result := 0;
  if A <> nil then
    Result := PDynArrayRec(NativeInt(A) - SizeOf(TDynArrayRec)).Length;
end;
{$ELSE}
asm
        TEST EAX, EAX
        JZ   @@exit
        MOV  EAX, [EAX-4]
@@exit:
end;
{$IFEND}

// Returns whether array is rectangular
function IsDynArrayRectangular(const DynArray: Pointer; typeInfo: PDynArrayTypeInfo): Boolean;
var
  Dim, I, J, Size, SubSize: Integer;
  P: Pointer;
begin
  // Assume we have a rectangular array
  Result := True;

  P := DynArray;
  Dim := DynArrayDim(typeInfo);

  {NOTE: Start at 1. Don't need to test the first dimension - it's rectangular by definition}
  for I := 1 to dim-1 do
  begin
    if P <> nil then
    begin
      { Get size of this dimension }
      Size := DynArraySize(P);

      { Get Size of first sub. dimension }
      SubSize := DynArraySize(PPointerArray(P)[0]);

      { Walk through every dimension making sure they all have the same size}
      for J := 1  to Size-1 do
        if DynArraySize(PPointerArray(P)[J]) <> SubSize then
        begin
          Result := False;
          Exit;
        end;

      { Point to next dimension}
      P := PPointerArray(P)[0];
    end;
  end;
end;

// Returns Bounds of Dynamic array as an array of integer containing the 'high' of each dimension
function DynArrayBounds(const DynArray: Pointer; typeInfo: PDynArrayTypeInfo): TBoundArray;
var
  Dim, I: Integer;
  P: Pointer;
begin
  P := DynArray;

  Dim := DynArrayDim(typeInfo);
  SetLength(Result, Dim);

  for I := 0 to dim-1 do
    if P <> nil then
    begin
      Result[I] := DynArraySize(P)-1;
      P := PPointerArray(P)[0]; // Assume rectangular arrays
    end;
end;

{ Decrements to next lower index - Returns True if successful }
{ Indices: Indices to be decremented }
{ Bounds : High bounds of each dimension }
function DecIndices(var Indices: TBoundArray; const Bounds: TBoundArray): Boolean;
var
  I, J: Integer;
begin
  { Find out if we're done: all at zeroes }
  Result := False;
  for I := Low(Indices)  to High(Indices) do
    if Indices[I] <> 0  then
    begin
      Result := True;
      break;
    end;
  if not Result then
    Exit;

  { Two arrays must be of same length }
  Assert(Length(Indices) = Length(Bounds));

  { Find index of item to tweak }
  for I := High(Indices) downto Low(Bounds) do
  begin
    // If not reach zero, dec and bail out
    if Indices[I] <> 0 then
    begin
      Dec(Indices[I]);
      Exit;
    end
    else
    begin
      J := I;
      while Indices[J] = 0 do
      begin
        // Restore high bound when we've reached zero on a particular dimension
        Indices[J] := Bounds[J];
        // Move to higher dimension
        Dec(J);
        Assert(J >= 0);
      end;
      Dec(Indices[J]);
      Exit;
    end;
  end;
end;

{ Package/Module registration/unregistration }

{$IFDEF MSWINDOWS}
const
  LCID_SUPPORTED          = $00000002;  { supported locale ids }
  LOCALE_SABBREVLANGNAME  = $00000003;  { abbreviated language name }
  LOCALE_SISO639LANGNAME  = $00000059;  { ISO abbreviated language name }
  LOCALE_SISO3166CTRYNAME = $0000005A;  { ISO abbreviated country name }
  LOCALE_SNAME            = $0000005c;  { locale name (ie: en-us) }
  LOCALE_SPARENT          = $0000006d;  { Fallback name for resources }
  LOCALE_NAME_MAX_LENGTH  = 85;
  MUI_LANGUAGE_ID         = $4;  { Use traditional language ID convention }
  MUI_LANGUAGE_NAME       = $8;  { Use ISO language (culture) name convention }
  MUI_UI_FALLBACK         = $30; { Retrieve a complete thread preferred UI languages list }
  LOAD_LIBRARY_AS_DATAFILE = 2;
  HKEY_CURRENT_USER = HKEY(NativeUInt($80000001));
  HKEY_LOCAL_MACHINE = HKEY(NativeUInt($80000002));
  KEY_ALL_ACCESS = $000F003F;
  KEY_READ = $000F0019;

  OlderLocaleOverrideKey = 'Software\Borland\Delphi\Locales'; // do not localize
  OldLocaleOverrideKey = 'Software\Borland\Locales'; // do not localize
  NewLocaleOverrideKey = 'Software\CodeGear\Locales'; // do not localize
  NewerLocaleOverrideKey = 'Software\Embarcadero\Locales'; // do not localize
{$ENDIF}

{$IF defined(CPU386)}
function FindModule(Instance: LongWord): PLibModule;
{$ELSE}
function FindModule(Instance: HINST): PLibModule;
{$IFEND}
begin
  Result := LibModuleList;
  while Result <> nil do
  begin
    if (Instance = Result.Instance) or
       (Instance = Result.CodeInstance) or
       (Instance = Result.DataInstance) or
       (Instance = Result.ResInstance) then
      Exit;
    Result := Result.Next;
  end;
end;

{$IF defined(CPU386)}
function FindHInstance(Address: Pointer): LongWord;
{$ELSE}
function FindHInstance(Address: Pointer): HINST;
{$IFEND}
{$IFDEF MSWINDOWS}
var
  MemInfo: TMemInfo;
begin
  VirtualQuery(Address, MemInfo, SizeOf(MemInfo));
  if MemInfo.State = $1000{MEM_COMMIT} then
    Result := NativeUInt(MemInfo.AllocationBase)
  else
    Result := 0;
end;
{$ENDIF}
{$IFDEF POSIX}
var
  Info: dl_info;
begin
  if (dladdr(NativeUInt(Address), Info) = 0) or (Info.dli_fbase = ExeBaseAddress) then
    Info.dli_fname := nil;   // if it's not in a library, assume the exe
  Result := NativeUInt(dlopen(PAnsiChar(UTF8Encode(Info.dli_fname)), RTLD_LAZY));
  if Result <> 0 then
    dlclose(Result);
end;
{$ENDIF}

{$IF defined(CPU386)}
function FindClassHInstance(ClassType: TClass): LongWord;
{$ELSE}
function FindClassHInstance(ClassType: TClass): HINST;
{$IFEND}
begin
  Result := FindHInstance(Pointer(ClassType));
end;

{$IFDEF POSIX}
function GetModuleFileName(Module: HMODULE; Buffer: PChar; BufLen: Integer): Integer;
var
  Addr: Pointer;
  Info: dl_info;
  FoundInModule: HMODULE;
{$IFDEF LINUX}
  Temp: Integer;
  ProcBuff: array [0..MAX_PATH] of AnsiChar;
{$ENDIF}
{$IFDEF MACOS}
  LoadedModule: HMODULE;
  Name: PAnsiChar;
  Index: Integer;
{$ENDIF MACOS}
begin
  Result := 0;
  if BufLen <= 0 then Exit;
  if (Module = MainInstance) or (Module = 0) then
  begin
    // First, try the dlsym approach.
    // dladdr fails to return the name of the main executable
    // in glibc prior to 2.1.91

{   Look for a dynamic symbol exported from this program.
    _DYNAMIC is not required in a main program file.
    If the main program is compiled with Delphi, it will always
    have a resource section, named @Sysinit@ResSym.
    If the main program is not compiled with Delphi, dlsym
    will search the global name space, potentially returning
    the address of a symbol in some other shared object library
    loaded by the program.  To guard against that, we check
    that the address of the symbol found is within the
    main program address range.  }

    dlerror;   // clear error state;  dlsym doesn't
//    Addr := dlsym(Module, '@Sysinit@ResSym');
    Addr := dlsym(Module, 'SysinitResSym');
    if (Addr <> nil) and (dlerror = nil)
      and (dladdr(NativeUInt(Addr), Info) <> 0)
      and (Info.dli_fname <> nil)
      and (Info.dli_fbase = ExeBaseAddress) then
    begin
      Result := strlen(Info.dli_fname);
      if Result >= BufLen then Result := BufLen-1;

      // dlinfo may not give a full path.  Compare to /proc/self/exe,
      // take longest result.
{$IFDEF LINUX}
//      Temp := readlink('/proc/self/exe', Buffer, BufLen);
      Temp := readlink('/proc/self/exe', ProcBuff, MAX_PATH);
      if Temp >= BufLen then Temp := BufLen-1;
      if Temp > Result then
      begin
        Utf8ToUnicode(Buffer, BufLen, ProcBuff, Temp);
        Result := Temp;
      end
      else
        Move(Info.dli_fname^, Buffer^, Result);
{$ENDIF LINUX}
{$IFDEF MACOS}
      Utf8ToUnicode(Buffer, BufLen, Info.dli_fname, Result);
{$ENDIF MACOS}
      Buffer[Result] := #0;
      Exit;
    end;

{$IFDEF LINUX}
    // Try inspecting the /proc/ virtual file system
    // to find the program filename in the process info
    Result := readlink(AnsiString('/proc/self/exe'), ProcBuff, MAX_PATH);
    if Result <> -1 then
    begin
      if Result >= BufLen then Result := BufLen-1;
      ProcBuff[Result] := #0;
      Utf8ToUnicode(Buffer, BufLen, ProcBuff, Result);
    end;
{$ENDIF LINUX}
{$IFDEF AllowParamStrModuleName}
{   Using ParamStr(0) to obtain a module name presents a potential
    security hole.  Resource modules are loaded based upon the filename
    of a given module.  We use dlopen() to load resource modules, which
    means the .init code of the resource module will be executed.
    Normally, resource modules contain no code at all - they're just
    carriers of resource data.
    An unpriviledged user program could launch our trusted,
    priviledged program with a bogus parameter list, tricking us
    into loading a module that contains malicious code in its
    .init section.
    Without this ParamStr(0) section, GetModuleFilename cannot be
    misdirected by unpriviledged code (unless the system program loader
    or the /proc file system or system root directory has been compromised).
    Resource modules are always loaded from the same directory as the
    given module.  Trusted code (programs, packages, and libraries)
    should reside in directories that unpriviledged code cannot alter.

    If you need GetModuleFilename to have a chance of working on systems
    where glibc < 2.1.91 and /proc is not available, and your
    program will not run as a priviledged user (or you don't care),
    you can define AllowParamStrModuleNames and rebuild the System unit
    and baseCLX package.  Note that even with ParamStr(0) support
    enabled, GetModuleFilename can still fail to find the name of
    a module.  C'est la Unix.  }

    if Result = -1 then // couldn't access the /proc filesystem
    begin               // return less accurate ParamStr(0)

{     ParamStr(0) returns the name of the link used
      to launch the app, not the name of the app itself.
      Also, if this app was launched by some other program,
      there is no guarantee that the launching program has set
      up our environment at all.  (example: Apache CGI) }

      if (ArgValues = nil) or (ArgValues^ = nil) or
        (PCharArray(ArgValues^)[0] = nil) then
      begin
        Result := 0;
        Exit;
      end;
      Result := strlen(PCharArray(ArgValues^)[0]);
      if Result >= BufLen then Result := BufLen-1;
      Move(PCharArray(ArgValues^)[0]^, Buffer^, Result);
      Buffer[Result] := #0;
    end;
{$ENDIF AllowParamStrModuleName}
  end
  else
  begin
{$IFDEF LINUX}
{   For shared object libraries, we can rely on the dlsym technique.
    Look for a dynamic symbol in the requested module.
    Don't assume the module was compiled with Delphi.
    We look for a dynamic symbol with the name _DYNAMIC.  This
    exists in all ELF shared object libraries that export
    or import symbols;  If someone has a shared object library that
    contains no imports or exports of any kind, this will probably fail.
    If dlsym can't find the requested symbol in the given module, it
    will search the global namespace and could return the address
    of a symbol from some other module that happens to be loaded
    into this process.  That would be bad, so we double check
    that the module handle of the symbol found matches the
    module handle we asked about.}

    dlerror;   // clear error state;  dlsym doesn't
    Addr := dlsym(Module, '_DYNAMIC');
    if (Addr <> nil) and (dlerror = nil)
      and (dladdr(NativeUInt(Addr), Info) <> 0) then
    begin
      if Info.dli_fbase = ExeBaseAddress then
        Info.dli_fname := nil;
      FoundInModule := HMODULE(dlopen(PAnsiChar(UTF8Encode(Info.dli_fname)), RTLD_LAZY));
      if FoundInModule <> 0 then
        dlclose(FoundInModule);
      if Module = FoundInModule then
      begin
        if Assigned(Info.dli_fname) then
        begin
          Result := strlen(Info.dli_fname);
          if Result >= BufLen then Result := BufLen-1;
          Move(Info.dli_fname^, Buffer^, Result);
        end
        else
          Result := 0;
        Buffer[Result] := #0;
      end;
    end;
{$ENDIF LINUX}
{$IFDEF MACOS}
{   Iterate through the loaded modules using _dyld_get_image_name,
    comparing those module handles to the handle provided.
    Note that this function is not thread safe as indicies into the
    loaded object list may change if other threads are loading or
    unloading modules. Most of the time this will not be the case;
    failure mode is to just return an empty string. }

    Index := 0;
    Name := _dyld_get_image_name(Index);
    while Name <> nil do
    begin
      LoadedModule := dlopen(Name, RTLD_LAZY);
      dlclose(LoadedModule);

      if LoadedModule = Module then
      begin
        Result := StrLen(Name);
        if Result >= BufLen then Result := BufLen - 1;
        Result := UTF8ToUnicode(Buffer, BufLen, Name, Result);
        Exit(Result);
      end;

      Inc(Index);
      Name := _dyld_get_image_name(Index);
    end;
    Result := 0;
{$ENDIF MACOS}
  end;

  if Result < 0 then Result := 0;
end;
{$ENDIF POSIX}

{$IF defined(CPU386)}
function DelayLoadResourceModule(Module: PLibModule): LongWord;
{$ELSE}
function DelayLoadResourceModule(Module: PLibModule): HINST;
{$IFEND}
var
  FileName: array[0..MAX_PATH] of Char;
begin
  if Module.ResInstance = 0 then
  begin
    GetModuleFileName(Module.Instance, FileName, SizeOf(FileName));
    Module.ResInstance := LoadResourceModule(FileName);
    if Module.ResInstance = 0 then
      Module.ResInstance := Module.Instance;
  end;
  Result := Module.ResInstance;
end;

{$IF defined(CPU386)}
function FindResourceHInstance(Instance: LongWord): LongWord;
{$ELSE}
function FindResourceHInstance(Instance: HINST): HINST;
{$IFEND}
var
  CurModule: PLibModule;
begin
  CurModule := LibModuleList;
  while CurModule <> nil do
  begin
    if (Instance = CurModule.Instance) or
       (Instance = CurModule.CodeInstance) or
       (Instance = CurModule.DataInstance) then
    begin
      Result := DelayLoadResourceModule(CurModule);
      Exit;
    end;
    CurModule := CurModule.Next;
  end;
  Result := Instance;
end;

{$IFDEF LINUX}
function GetUILanguages(const LANGID: WORD): string;
var
  Lang: AnsiString;
  Ind: integer;
  languagePart: string;
begin
  // language[_territory][.codeset][@modifiers]
  // language and territory shall consist of LETTERS only.
  Lang := AnsiString(getenv('LANG'));
  Result := '';
  if Lang = '' then exit;

  languagePart := '';

  for ind := 1 to length(Lang) do
  begin
    if not(Lang[Ind] in ['a'..'z', 'A'..'Z', '_']) then
      break;
    if Lang[Ind] = '_' then languagePart := Result;
    Result := Result + WideChar(Lang[Ind]);
  end;

  if languagePart <> '' then
    Result :=  Result + ',' + languagePart;
end;

function InternalGetLocaleOverride(AppName: string): string;
begin
  Result := ''; // no override mechanism
end;
{$ENDIF LINUX}
{$IFDEF MACOS}
function StringRefToString(StringRef: CFStringRef): string;
var
  Range: CFRange;
begin
  Range.location := 0;
  Range.length := CFStringGetLength(StringRef);
  if Range.length > 0 then
  begin
    SetLength(Result, Range.length);
    CFStringGetCharacters(StringRef, Range, @Result[1]);
  end
  else
    Result := '';
end;

function GetUILanguages(const LANGID: WORD): string;
var
  PL : CFArrayRef;
  I: integer;
begin
  Result := '';
  PL := CFLocaleCopyPreferredLanguages;
  try
    if CFArrayGetCount(PL) > 0 then
    begin
      Result := StringRefToString(CFArrayGetValueAtIndex(PL, 0));
      for I := 1 to CFArrayGetCount(PL)-1 do
        Result := Result + ',' + StringRefToString(CFArrayGetValueAtIndex(PL, I));
    end;
  finally
    CFRelease(PL);
  end;
end;

function InternalGetLocaleOverride(AppName: string): string;
begin
  Result := ''; // no override mechanism
end;

{$ENDIF MACOS}

{$IFDEF MSWINDOWS}
type
  TLanguageEntry = record
    ID: WORD;
    List: PAnsiChar;
  end;

{$I LocaleData.INC }

var
  GetThreadPreferredUILanguages : function(dwFlags: LONGWORD; pulNumLanguages: Pointer;
    pwszLanguagesBuffer: PWideChar; pcchLanguagesBuffer: Pointer): Boolean; stdcall;
  SetThreadPreferredUILanguages : function(dwFlags: LONGWORD; pwszLanguagesBuffer: Pointer;
    pulNumLanguages: Pointer): Boolean; stdcall;
  GetThreadUILanguage : function : WORD; stdcall;
  UseThreadUILanguageAPI: Boolean;
  CrSec: CriticalSection;
  CachedLangID: Word;
  CachedLanguageNames: array[0.. LOCALE_NAME_MAX_LENGTH-1] of Char;

procedure InitializeLocaleData;
begin
  InitializeCriticalSection(CrSec);
  CachedLangID := $7f; //  LANG_INVARIANT
  UseThreadUILanguageAPI := (GetVersion and $000000FF) >= 6;
  if UseThreadUILanguageAPI then
  begin
    @GetThreadPreferredUILanguages := GetProcAddress(GetModuleHandle(kernel), 'GetThreadPreferredUILanguages');
    @SetThreadPreferredUILanguages := GetProcAddress(GetModuleHandle(kernel), 'SetThreadPreferredUILanguages');
    @GetThreadUILanguage:= GetProcAddress(GetModuleHandle(kernel), 'GetThreadUILanguage');
  end;
end;

procedure FinalizeLocaleDate;
begin
  DeleteCriticalSection(CrSec);
end;

function GetUILanguages(const LANGID: WORD): string;

  function LastHyphenPos(S : String) : integer;
  var
    I: integer;
  begin
    for I := Length(S) downto 1 do
      if S[I] = '-' then exit (I-1);
    Result := 0;
  end;

  function ConvertResToUILanguages(ResBuffer: PAnsiChar): String;
  var
    I: Integer;
    Separator,
    ALanguage: String;
  begin
    Result := String(PAnsiChar(ResBuffer));
    for I := 1 to Length(Result) do
      if Result[I] = ',' then exit;
    ALanguage := Result;
    Result := '';
    while ALanguage <> '' do
    begin
      Result := Result + Separator + ALanguage;
      Separator := ',';
      ALanguage := Copy(ALanguage, 1, LastHyphenPos(ALanguage));
    end;
  end;

  function GetPreferredLangForOldOS(LANGID: Word): string;
  var
    Language, Region : array[0.. LOCALE_NAME_MAX_LENGTH-1] of Char;
    H, L, I: Cardinal;
  begin
    Result := '';
    // Lookup exceptional languages table.
    if (NumberOfLocaleData > 0) and (LocaleTable[0].ID <= LANGID) and (LANGID <= LocaleTable[NumberOfLocaleData-1].ID) then
    begin
      H := NumberOfLocaleData-1;
      L := 0;
      while H >= L do
      begin
        I := (H + L) div 2;
        if LocaleTable[I].ID > LANGID then H := I - 1
        else if LocaleTable[I].ID < LANGID then L :=  I + 1
        else
        begin
          Result := ConvertResToUILanguages(LocaleTable[I].List);
          Break;
        end;
      end;
    end;
    if (Result = '') and IsValidLocale(LANGID, LCID_SUPPORTED) then
    begin
      // Generate language names: <language>-<country> and <language>
      GetLocaleInfo(LANGID, LOCALE_SISO639LANGNAME, Language, LOCALE_NAME_MAX_LENGTH);
      GetLocaleInfo(LANGID, LOCALE_SISO3166CTRYNAME, Region, LOCALE_NAME_MAX_LENGTH);
      Result := String(Language) + '-' + String(Region) + ',' + String(Language);
    end;
  end;

  function CheckDifferentLanguageList(src1, src2: PWideChar; len: integer): boolean;
  begin
    Result := True;
    while len > 0 do
    begin
      if (src1^ <> src2^) then exit;
      inc(src1);
      inc(src2);
      dec(len);
    end;
    Result := False;
  end;

  function ThreadUILanguages(var bufsize: Integer): PWideChar;
  var
    I: Integer;
  begin
    Result := nil;
    bufsize := 0;
    if GetThreadPreferredUILanguages(MUI_LANGUAGE_NAME or MUI_UI_FALLBACK, @I, nil, @bufsize) then
    begin
      GetMem(Result, bufsize * sizeof(Char));
      GetThreadPreferredUILanguages(MUI_LANGUAGE_NAME or MUI_UI_FALLBACK, @I, Result, @bufsize);
    end;
  end;

  function GetPreferredLangForNewOS(const LANGID: WORD): string;
  var
    SavedBufSize, BufSize: Integer;
    SavedUILanguages, UILanguages: PChar;
    I: integer;
    W: WORD;
    IDBuf: array[0..5] of WideChar; // four digits + two #0
  begin
    SavedUILanguages := nil;
    if GetThreadUILanguage <> LANGID then
    begin
      SavedUILanguages := ThreadUILanguages(SavedBufSize);
      W := LANGID;
      for I := 3 downto 0 do
      begin
        IDBuf[I] := WideChar(Ord(hexDigits[W and $0F]));
        W := W div 16;
      end;
      IDBuf[4] := #0; // Double null-terminator.
      IDBuf[5] := #0;
      SetThreadPreferredUILanguages(MUI_LANGUAGE_ID, @IDBuf, @I);
    end;

    UILanguages := ThreadUILanguages(BufSize);
    if UILanguages <> nil then
    begin
      for I := 0 to BufSize - 2 do
        if UILanguages[I] = #0 then UILanguages[I] := ',';
      Result := UILanguages;
      FreeMem(UILanguages);
    end;

    if SavedUILanguages <> nil then
    begin
      SetThreadPreferredUILanguages(0, nil, @I);
      UILanguages := ThreadUILanguages(BufSize);
      if (SavedBufSize <> BufSize) or CheckDifferentLanguageList(SavedUILanguages, UILanguages, BufSize) then
        SetThreadPreferredUILanguages(MUI_LANGUAGE_NAME, SavedUILanguages, @I);
      FreeMem(UILanguages);
      FreeMem(SavedUILanguages);
    end;
  end;

begin
  EnterCriticalSection(CrSec);
  if CachedLangID = LANGID then
  begin
    Result := CachedLanguageNames;
    LeaveCriticalSection(CrSec);
    exit;
  end;
  LeaveCriticalSection(CrSec);

  Result := '';
  if IsValidLocale(LANGID, LCID_SUPPORTED) then
  begin
    if UseThreadUILanguageAPI then
      Result := GetPreferredLangForNewOS(LANGID)
    else
    begin
      Result := GetPreferredLangForOldOS(LANGID);
      if LangID <> GetSystemDefaultUILanguage then
      begin
        if Result <> '' then Result := Result + ',';
        Result := Result + GetPreferredLangForOldOS(GetSystemDefaultUILanguage);
      end;
    end;
  end;

  EnterCriticalSection(CrSec);
  CachedLangID := LANGID;
  lstrcpyn(CachedLanguageNames, PChar(Result), SizeOf(CachedLanguageNames));
  LeaveCriticalSection(CrSec);
end;

function InternalGetLocaleOverride(AppName: string): string;

  function FindBS(Current: PChar): PChar;
  begin
    Result := Current;
    while (Result^ <> #0) and (Result^ <> '\') do
      Result := CharNext(Result);
  end;

  function ToLongPath(AFileName: PChar; BufLen: Integer): PChar;
  var
    CurrBS, NextBS: PChar;
    {$IFDEF CPU386}
    Module: Integer;
    {$ELSE}
    Module: HMODULE;
    {$ENDIF}
    Handle: THandle;
    L: Integer;
    FindData: TWin32FindData;
    Buffer: array[0..MAX_PATH] of Char;
    GetLongPathName: function (ShortPathName: PChar; LongPathName: PChar;
      cchBuffer: Integer): Integer stdcall;
  const
    longPathName = 'GetLongPathNameW';
  begin
    Result := AFileName;
    Module := GetModuleHandle(kernel);
    if Module <> 0 then
    begin
      @GetLongPathName := GetProcAddress(Module, longPathName);
      if Assigned(GetLongPathName) and
        (GetLongPathName(AFileName, Buffer, Length(Buffer)) <> 0) then
      begin
        lstrcpyn(AFileName, Buffer, BufLen);
        Exit;
      end;
    end;

    if AFileName[0] = '\' then
    begin
      if AFileName[1] <> '\' then Exit;
      CurrBS := FindBS(AFileName + 2);  // skip server name
      if CurrBS^ = #0 then Exit;
      CurrBS := FindBS(CurrBS + 1);     // skip share name
      if CurrBS^ = #0 then Exit;
    end else
      CurrBS := AFileName + 2;          // skip drive name

    L := CurrBS - AFileName;
    lstrcpyn(Buffer, AFileName, L + 1);
    while CurrBS^ <> #0 do
    begin
      NextBS := FindBS(CurrBS + 1);
      if L + (NextBS - CurrBS) + 1 > Length(Buffer) then Exit;
      lstrcpyn(Buffer + L, CurrBS, (NextBS - CurrBS) + 1);

      Handle := FindFirstFile(Buffer, FindData);
      if Handle = THandle(-1) then Exit;
      FindClose(Handle);

      if L + 1 + _strlen(FindData.cFileName) + 1 > Length(Buffer) then Exit;
      Buffer[L] := '\';
      lstrcpyn(Buffer + L + 1, FindData.cFileName, Length(Buffer) - L - 1);
      Inc(L, _strlen(FindData.cFileName) + 1);
      CurrBS := NextBS;
    end;
    lstrcpyn(AFileName, Buffer, BufLen);
  end;

var
  HostAppName: array [0..MAX_PATH] of Char;
  LocaleOverride: PChar;
  Key: HKEY;
  LocSize: Integer;
begin
  if AppName = '' then
    GetModuleFileName(0, HostAppName, Length(HostAppName)) // Get host application name
  else
    lstrcpyn(HostAppName, PChar(AppName), Length(HostAppName));
  if HostAppName[0] = #$0 then exit;
  LocaleOverride := nil;

  if (RegOpenKeyEx(HKEY_CURRENT_USER, NewerLocaleOverrideKey, 0, KEY_READ, Key) = 0) or
   (RegOpenKeyEx(HKEY_LOCAL_MACHINE, NewerLocaleOverrideKey, 0, KEY_READ, Key) = 0) or
   (RegOpenKeyEx(HKEY_CURRENT_USER, NewLocaleOverrideKey, 0, KEY_READ, Key) = 0) or
   (RegOpenKeyEx(HKEY_LOCAL_MACHINE, NewLocaleOverrideKey, 0, KEY_READ, Key) = 0) or
   (RegOpenKeyEx(HKEY_CURRENT_USER, OldLocaleOverrideKey, 0, KEY_READ, Key) = 0) or
   (RegOpenKeyEx(HKEY_CURRENT_USER, OlderLocaleOverrideKey, 0, KEY_READ, Key) = 0) then
  try
    ToLongPath(HostAppName, Length(HostAppName));
    if RegQueryValueEx(Key, HostAppName, nil, nil, nil, @LocSize) = 0 then
    begin
      GetMem(LocaleOverride, LocSize);
      RegQueryValueEx(Key, HostAppName, nil, nil, LocaleOverride, @LocSize);
      Result := LocaleOverride;
    end
    else if RegQueryValueEx(Key, '', nil, nil, nil, @LocSize) = 0 then
    begin
      GetMem(LocaleOverride, LocSize);
      RegQueryValueEx(Key, '', nil, nil, LocaleOverride, @LocSize);
      Result := LocaleOverride;
    end;
  finally
    if LocaleOverride <> nil then
      FreeMem(LocaleOverride);
    RegCloseKey(Key);
  end;
end;

{$ENDIF MSWINDOWS}

var
  PreferredLanguagesOverride: PChar = nil;

function GetLocaleOverride(const AppName: string): string;
begin
  if PreferredLanguagesOverride = nil then
    SetLocaleOverride(InternalGetLocaleOverride(AppName));
  Result := PreferredLanguagesOverride;
end;

procedure SetLocaleOverride(const NewPreferredLanguages: string);
var
  L: Integer;
begin
  if PreferredLanguagesOverride <> nil then
    FreeMem(PreferredLanguagesOverride);
  L := Length(NewPreferredLanguages);
  if L > 0 then
  begin
    Inc(L);
    GetMem(PreferredLanguagesOverride, L * SizeOf(Char));
    MoveChars(NewPreferredLanguages[1], PreferredLanguagesOverride^, L);
  end;
end;

function LoadModule(ModuleName: string; CheckOwner: Boolean): LongWord;
{$IFDEF MSWINDOWS}
begin
    Result := LoadLibraryEx(PChar(ModuleName), 0, LOAD_LIBRARY_AS_DATAFILE)
end;
{$ENDIF}
{$IFDEF POSIX}
var
  FileName: UTF8String;
  begin
  FileName := UTF8Encode(ModuleName);
  Result := dlopen(PAnsiChar(FileName), RTLD_LAZY);
  end;
{$ENDIF}

function GetResourceModuleName(HostAppName, ModuleName: string): string;

  function ResouceDLLExists(S: string): Boolean;
{$IFDEF POSIX}
  var
    st2: _stat;
    FileName: UTF8String;
  begin
    Result := False;
    FileName := UTF8Encode(S);
    if (stat(PAnsiChar(FileName), st2) = -1) then
      exit;
   // Need Security check.
    Result := true;
end;
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
var
    Handle: THandle;
    FindData: TWin32FindData;
  begin
    Handle := FindFirstFile(PWideChar(S), FindData);
    Result := Handle <> THandle(-1);
    if Result then
      FindClose(Handle);
  end;
{$ENDIF}

  function LoadLanguageList(FileNameBody, List: String): string;
  Var
    s, ind : integer;
  begin
    Result := '';
    ind := 1;
    while (ind <= length(List)) do
    begin
      s := ind;
      while (ind <= Length(List)) and (List[ind] <> ',') do inc(ind);
      if s <> ind then
      begin
        Result := FileNameBody + Copy(List, s, ind-s);
        if ResouceDLLExists(Result) then exit;
      end;
      inc(ind);
    end;
    Result := '';
  end;

{$IFDEF MSWINDOWS}
  function Load3LettersModule(FileNameBody: string): string;
  var
    ExtPart : array[0..3] of char;
  begin
    GetLocaleInfo(GetUserDefaultUILanguage, LOCALE_SABBREVLANGNAME, ExtPart, sizeof(ExtPart));
    Result := FileNameBody + string(ExtPart);
    if ResouceDLLExists(Result) then Exit;
    ExtPart[2] := #$0;
    Result := FileNameBody + string(ExtPart);
    if ResouceDLLExists(Result) then Exit;
    Result := '';
  end;
{$ENDIF MSWINDOWS}

var
  LocaleOverrideKey: string;
  FileNameBody : string;
{$IFDEF MSWINDOWS}
  Ind: integer;
{$ENDIF}
begin
  Result := '';
{$IFDEF MSWINDOWS}
  FileNameBody := ModuleName;
  for ind := Length(ModuleName) downto 1 do
  begin
    if ModuleName[ind] = '.' then
    begin
      FileNameBody := Copy(ModuleName, 1, ind);
      break;
    end;
  end;
{$ENDIF}
{$IFDEF POSIX}
  FileNameBody := ModuleName + '.';
{$ENDIF}
  LocaleOverrideKey := GetLocaleOverride(HostAppName);
  if LocaleOverrideKey <> '' then
    Result := LoadLanguageList(FileNameBody, LocaleOverrideKey)
  else
  begin
{$IFDEF POSIX}
    Result := LoadLanguageList(FileNameBody, GetUILanguages(0)); // 0 is dummy argument.
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
    Result := LoadLanguageList(FileNameBody, GetUILanguages(GetUserDefaultUILanguage));
    if (Result = '') and (not UseThreadUILanguageAPI) then
      Result := LoadLanguageList(FileNameBody, GetUILanguages(GetSystemDefaultUILanguage));
    if Result = '' then
      Result := Load3LettersModule(FileNameBody);
{$ENDIF}
  end;
end;

function LoadResourceModule(ModuleName: PChar; CheckOwner: Boolean): LongWord;
var
  HostAppName: array [0..MAX_PATH] of Char;
  ResModuleName : string;
begin
  Result := 0;
  GetModuleFileName(0, HostAppName, Length(HostAppName));
  ResModuleName := GetResourceModuleName(HostAppName, ModuleName);
  if ResModuleName <> '' then
    Result := LoadModule(ResModuleName, CheckOwner);
end;

procedure EnumModules(Func: TEnumModuleFunc; Data: Pointer); assembler;
begin
  EnumModules(TEnumModuleFuncLW(Func), Data);
end;

procedure EnumResourceModules(Func: TEnumModuleFunc; Data: Pointer);
begin
  EnumResourceModules(TEnumModuleFuncLW(Func), Data);
end;

procedure EnumModules(Func: TEnumModuleFuncLW; Data: Pointer);
var
  CurModule: PLibModule;
begin
  CurModule := LibModuleList;
  while CurModule <> nil do
  begin
    if not Func(CurModule.Instance, Data) then Exit;
    CurModule := CurModule.Next;
  end;
end;

procedure EnumResourceModules(Func: TEnumModuleFuncLW; Data: Pointer);
var
  CurModule: PLibModule;
begin
  CurModule := LibModuleList;
  while CurModule <> nil do
  begin
    if not Func(DelayLoadResourceModule(CurModule), Data) then Exit;
    CurModule := CurModule.Next;
  end;
end;

procedure AddModuleUnloadProc(Proc: TModuleUnloadProc);
begin
  AddModuleUnloadProc(TModuleUnloadProcLW(Proc));
end;

procedure RemoveModuleUnloadProc(Proc: TModuleUnloadProc);
begin
  RemoveModuleUnloadProc(TModuleUnloadProcLW(Proc));
end;

procedure AddModuleUnloadProc(Proc: TModuleUnloadProcLW);
var
  P: PModuleUnloadRec;
begin
  New(P);
  P.Next := ModuleUnloadList;
  @P.Proc := @Proc;
  ModuleUnloadList := P;
end;

procedure RemoveModuleUnloadProc(Proc: TModuleUnloadProcLW);
var
  P, C: PModuleUnloadRec;
begin
  P := ModuleUnloadList;
  if (P <> nil) and (@P.Proc = @Proc) then
  begin
    ModuleUnloadList := ModuleUnloadList.Next;
    Dispose(P);
  end else
  begin
    C := P;
    while C <> nil do
    begin
      if (C.Next <> nil) and (@C.Next.Proc = @Proc) then
      begin
        P := C.Next;
        C.Next := C.Next.Next;
        Dispose(P);
        Break;
      end;
      C := C.Next;
    end;
  end;
end;

procedure NotifyModuleUnload(HInstance: LongWord);
var
  P: PModuleUnloadRec;
begin
  P := ModuleUnloadList;
  while P <> nil do
  begin
    try
      P.Proc(HInstance);
    except
      // Make sure it doesn't stop notifications
    end;
    P := P.Next;
  end;
{$IFDEF LINUX}
  InvalidateModuleCache;
{$ENDIF}
end;

procedure RegisterModule(LibModule: PLibModule);
begin
  LibModule.Next := LibModuleList;
  LibModuleList := LibModule;
end;

procedure UnregisterModule(LibModule: PLibModule);
var
  CurModule: PLibModule;
begin
  try
    NotifyModuleUnload(LibModule.Instance);
  finally
    if LibModule = LibModuleList then
      LibModuleList := LibModule.Next
    else
    begin
      CurModule := LibModuleList;
      while CurModule <> nil do
      begin
        if CurModule.Next = LibModule then
        begin
          CurModule.Next := LibModule.Next;
          Break;
        end;
        CurModule := CurModule.Next;
      end;
    end;
  end;
end;




function _IntfClear(var Dest: IInterface): Pointer;
{$IFDEF PUREPASCAL}
var
  P: Pointer;
begin
  Result := @Dest;
  if Dest <> nil then
  begin
    P := Pointer(Dest);
    Pointer(Dest) := nil;
    IInterface(P)._Release;
  end;
end;
{$ELSE}
asm
        MOV     EDX,[EAX]
        TEST    EDX,EDX
        JE      @@1
        MOV     DWORD PTR [EAX],0
{$IFDEF ALIGN_STACK}
        SUB     ESP, 4
{$ENDIF ALIGN_STACK}
        PUSH    EAX
        PUSH    EDX
        MOV     EAX,[EDX]
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface._Release
        POP     EAX
{$IFDEF ALIGN_STACK}
        ADD     ESP, 4
{$ENDIF ALIGN_STACK}
@@1:
end;
{$ENDIF !PUREPASCAL}




procedure _IntfCopy(var Dest: IInterface; const Source: IInterface);
{$IFDEF PUREPASCAL}
var
  P: Pointer;
begin
  P := Pointer(Dest);
  if Source <> nil then
    Source._AddRef;
  Pointer(Dest) := Pointer(Source);
  if P <> nil then
    IInterface(P)._Release;
end;
{$ELSE}
asm
{
  The most common case is the single assignment of a non-nil interface
  to a nil interface.  So we streamline that case here.  After this,
  we give essentially equal weight to other outcomes.

    The semantics are:  The source intf must be addrefed *before* it
    is assigned to the destination.  The old intf must be released
    after the new intf is addrefed to support self assignment (I := I).
    Either intf can be nil.  The first requirement is really to make an
    error case function a little better, and to improve the behaviour
    of multithreaded applications - if the addref throws an exception,
    you don't want the interface to have been assigned here, and if the
    assignment is made to a global and another thread references it,
    again you don't want the intf to be available until the reference
    count is bumped.
}
        TEST    EDX,EDX         // is source nil?
        JE      @@NilSource
        PUSH    EDX             // save source
        PUSH    EAX             // save dest
        MOV     EAX,[EDX]       // get source vmt
        PUSH    EDX             // source as arg
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface._AddRef
        POP     EAX             // retrieve dest
        MOV     ECX, [EAX]      // get current value
        POP     [EAX]           // set dest in place
        TEST    ECX, ECX        // is current value nil?
        JNE     @@ReleaseDest   // no, release it
        RET                     // most common case, we return here
@@ReleaseDest:
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        MOV     EAX,[ECX]       // get current value vmt
        PUSH    ECX             // current value as arg
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface._Release
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
        RET

{   Now we're into the less common cases.  }
@@NilSource:
        MOV     ECX, [EAX]      // get current value
        TEST    ECX, ECX        // is it nil?
        MOV     [EAX], EDX      // store in dest (which is nil)
        JE      @@Done
        MOV     EAX, [ECX]      // get current vmt
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    ECX             // current value as arg
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface._Release
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
@@Done:
end;
{$ENDIF !PUREPASCAL}



procedure _IntfCast(var Dest: IInterface; const Source: IInterface; const IID: TGUID);
{$IFDEF PUREPASCAL}
// PIC:  EBX must be correct before calling QueryInterface
var
  Temp: IInterface;
begin
  if Source = nil then
    Dest := nil
  else
  begin
    Temp := nil;
    if Source.QueryInterface(IID, Temp) <> 0 then
      Error(reIntfCastError)
    else
      Dest := Temp;
  end;
end;
{$ELSE}
asm
        TEST    EDX,EDX
        JE      _IntfClear
{$IFDEF ALIGN_STACK}
        SUB     ESP, 8
{$ENDIF ALIGN_STACK}
        PUSH    EDI
        MOV     EDI, EAX   // ptr to dest
        PUSH    0
        PUSH    ESP        // ptr to temp
        PUSH    ECX        // ptr to GUID
        PUSH    EDX        // ptr to source
@@1:    MOV     EAX,[EDX]
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface.QueryInterface
        TEST    EAX,EAX
        JE      @@2
{$IFDEF ALIGN_STACK}
        ADD     ESP, 16
{$ENDIF ALIGN_STACK}
        MOV     AL,reIntfCastError
        JMP     Error
@@2:    MOV     EAX, [EDI]
        TEST    EAX, EAX
        JE      @@3
        PUSH    EAX
        MOV     EAX,[EAX]
        CALL    DWORD PTR [EAX] + VMTOFFSET IInterface._Release
@@3:    POP     EAX          // value of temp
        MOV     [EDI], EAX
        POP     EDI
{$IFDEF ALIGN_STACK}
        ADD     ESP, 8
{$ENDIF ALIGN_STACK}
end;
{$ENDIF !PUREPASCAL}

procedure _IntfAddRef(const Dest: IInterface);
begin
  if Dest <> nil then Dest._AddRef;
end;

procedure TInterfacedObject.AfterConstruction;
begin
// Release the constructor's implicit refcount
  InterlockedDecrement(FRefCount);
end;

procedure TInterfacedObject.BeforeDestruction;
begin
  if RefCount <> 0 then
    Error(reInvalidPtr);
end;

// Set an implicit refcount so that refcounting
// during construction won't destroy the object.
class function TInterfacedObject.NewInstance: TObject;
begin
  Result := inherited NewInstance;
  TInterfacedObject(Result).FRefCount := 1;
end;

function TInterfacedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TInterfacedObject._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TInterfacedObject._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    Destroy;
end;

{ TAggregatedObject }

constructor TAggregatedObject.Create(const Controller: IInterface);
begin
  // weak reference to controller - don't keep it alive
  FController := Pointer(Controller);
end;

function TAggregatedObject.GetController: IInterface;
begin
  Result := IInterface(FController);
end;

function TAggregatedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := IInterface(FController).QueryInterface(IID, Obj);
end;

function TAggregatedObject._AddRef: Integer;
begin
  Result := IInterface(FController)._AddRef;
end;

function TAggregatedObject._Release: Integer; stdcall;
begin
  Result := IInterface(FController)._Release;
end;

{ TContainedObject }

function TContainedObject.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

{ TClassHelperBase }

constructor TClassHelperBase._Create(Instance: TObject);
begin
  inherited Create;
  FInstance := Instance;
end;



{$IFOPT O+}
  // Turn off optimizations to force creating a EBP stack frame and
  // place params on the stack.
  {$DEFINE OPTIMIZATIONSON}
  {$O-}
{$ENDIF}
function _CheckAutoResult(ResultCode: HResult): HResult;
var
  ReturnAddr: Pointer;
begin
  if ResultCode < 0 then
  begin
    ReturnAddr := PPointer(PByte(@ResultCode) + SizeOf(ResultCode) + SizeOf(Pointer))^;
    if Assigned(SafeCallErrorProc) then
      SafeCallErrorProc(ResultCode, ReturnAddr);
    ErrorAt(Byte(reSafeCallError), ReturnAddr);
  end;
  Result := ResultCode;
end;
{$IFDEF OPTIMIZATIONSON}
  {$UNDEF OPTIMIZATIONSON}
  {$O+}
{$ENDIF}

function  CompToDouble(Value: Comp): Double; cdecl;
begin
  Result := Value;
end;

procedure  DoubleToComp(Value: Double; var Result: Comp); cdecl;
begin
  Result := Value;
end;

function  CompToCurrency(Value: Comp): Currency; cdecl;
begin
  Result := Value;
end;

procedure  CurrencyToComp(Value: Currency; var Result: Comp); cdecl;
begin
  Result := Value;
end;

function GetMemory(Size: Integer): Pointer; cdecl;
begin
  Result := MemoryManager.GetMem(Size);
end;

function FreeMemory(P: Pointer): Integer; cdecl;
begin
  if P = nil then
    Result := 0
  else
    Result := MemoryManager.FreeMem(P);
end;

function ReallocMemory(P: Pointer; Size: Integer): Pointer; cdecl;
begin
  if P = nil then
    Result := GetMemory(Size)
  else
  Result := MemoryManager.ReallocMem(P, Size);
end;

// UnicodeToUTF8(3):
// Scans the source data to find the null terminator, up to MaxBytes
// Dest must have MaxBytes available in Dest.

function UnicodeToUtf8(Dest: PAnsiChar; Source: PWideChar; MaxBytes: Integer): Integer;
begin
  Result := UnicodeToUtf8(Dest, MaxBytes, Source, Cardinal(-1));
end;

// UnicodeToUtf8(4):
// MaxDestBytes includes the null terminator (last char in the buffer will be set to null)
// Function result includes the null terminator.
// Nulls in the source data are not considered terminators - SourceChars must be accurate


function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
begin
  Result := 0;
  if Source = nil then Exit;
  if Dest <> nil then
  begin
    Result := Cardinal(LocaleCharsFromUnicode(CP_UTF8, 0, Source, Integer(SourceChars), Dest, Integer(MaxDestBytes), nil, nil));
    if Result = MaxDestBytes then
    begin
      while (Result > 1) and (Byte(Dest[Result - 1]) > $7F) and (Byte(Dest[Result - 1]) and $80 <> 0) and (Byte(Dest[Result - 1]) and $C0 <> $C0) do
        Dec(Result);
    end
  end else
    Result := Cardinal(LocaleCharsFromUnicode(CP_UTF8, 0, Source, Integer(SourceChars), nil, 0, nil, nil));
end;

function Utf8ToUnicode(Dest: PWideChar; Source: PAnsiChar; MaxChars: Integer): Integer;
begin
  Result := Utf8ToUnicode(Dest, MaxChars, Source, Cardinal(-1));
end;


function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PAnsiChar; SourceBytes: Cardinal): Cardinal;
begin
  Result := 0;
  if Source = nil then Exit;
  if (Dest <> nil) and (MaxDestChars > 0) then
  begin
    Result := Cardinal(UnicodeFromLocaleChars(CP_UTF8, 0, Source, Integer(SourceBytes), Dest, Integer(MaxDestChars)));
    if Result = MaxDestChars then
    begin
      if (Result > 1) and (Word(Dest[Result - 1]) >= $DC00) and (Word(Dest[Result - 1]) <= $DFFF) then
        Dec(Result);
    end;
  end else
    Result := Cardinal(UnicodeFromLocaleChars(CP_UTF8, 0, Source, Integer(SourceBytes), nil, 0));
end;

function Utf8Encode(const WS: WideString): RawByteString;
var
  L: Integer;
  Temp: UTF8String;
begin
  Result := '';
  if WS = '' then Exit;
  L := Length(WS);
  SetLength(Temp, L * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PAnsiChar(Temp), Length(Temp) + 1, PWideChar(WS), L);
  if L > 0 then
    SetLength(Temp, L)
  else
    Temp := '';
  Result := Temp;
  if Result <> '' then
    PStrRec(NativeInt(Result) - SizeOf(StrRec)).codePage := CP_UTF8;
end;

function Utf8Encode(const US: UnicodeString): RawByteString;
var
  L: Integer;
  Temp: UTF8String;
begin
  Result := '';
  if US = '' then Exit;
  L := Length(US);
  SetLength(Temp, L * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PAnsiChar(Temp), Length(Temp) + 1, PWideChar(US), L);
  if L > 0 then
    SetLength(Temp, L)
  else
    Temp := '';
  Result := Temp;
  if Result <> '' then
    PStrRec(NativeInt(Result) - SizeOf(StrRec)).codePage := CP_UTF8;
end;

function UTF8Encode(const A: RawByteString): RawByteString;
begin
  if StringCodePage(A) = CP_UTF8 then
    Result := A
  else
    Result := UTF8Encode(UnicodeString(A));
end;

function Max(I1, I2: Integer): Integer; inline;
begin
  if I1 > I2 then
    Result := I1
  else
    Result := I2;
end;

function UTF8EncodeToShortString(const WS: WideString): ShortString;
begin
  Result[0] := AnsiChar(Max(0, UnicodeToUtf8(@Result[1], High(Result), PWideChar(WS), Length(WS))));
end;

function UTF8EncodeToShortString(const US: UnicodeString): ShortString;
begin
  Result[0] := AnsiChar(Max(0, UnicodeToUtf8(@Result[1], High(Result), PWideChar(US), Length(US))));
end;

function UTF8EncodeToShortString(const A: RawByteString): ShortString;
begin
  if StringCodePage(A) = CP_UTF8 then
    Result := A
  else
    Result := UTF8EncodeToShortString(UnicodeString(A));
end;

function Utf8Decode(const S: RawByteString): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then Exit;
  L := Length(S);
  SetLength(Temp, L);

  L := Utf8ToUnicode(PWideChar(Temp), L + 1, PAnsiChar(S), L);
  if L > 0 then
    SetLength(Temp, L)
  else
    Temp := '';
  Result := Temp;
end;

function UTF8ToWideString(const S: RawByteString): WideString; inline;
begin
  Result := UTF8Decode(S);
end;

function UTF8ToUnicodeString(const S: RawByteString): UnicodeString;
var
  L: Integer;
  Temp: UnicodeString;
begin
  Result := '';
  if S = '' then Exit;
  L := Length(S);
  SetLength(Temp, L);

  L := Utf8ToUnicode(PWideChar(Temp), L + 1, PAnsiChar(S), L);
  if L > 0 then
    SetLength(Temp, L)
  else
    Temp := '';
  Result := Temp;
end;

function UTF8ToUnicodeString(const S: PAnsiChar): UnicodeString; overload;
var
  L: Integer;
  Temp: UnicodeString;
begin
  Result := '';
  if S = '' then Exit;
  L := Length(S);
  SetLength(Temp, L);

  L := Utf8ToUnicode(PWideChar(Temp), L + 1, S, L);
  if L > 0 then
    SetLength(Temp, L)
  else
    Temp := '';
  Result := Temp;
end;

function UTF8ToUnicodeString(const S: ShortString): UnicodeString; overload;
var
  L: Integer;
  Temp: UnicodeString;
begin
  Result := '';
  if S = '' then Exit;
  L := Length(S);
  SetLength(Temp, L);

  L := Utf8ToUnicode(PWideChar(Temp), L + 1, @S[1], L);
  if L > 0 then
    SetLength(Temp, L)
  else
    Temp := '';
  Result := Temp;
end;

function UTF8ToString(const S: RawByteString): string;
begin
  Result := UTF8ToUnicodeString(S);
end;

function UTF8ToString(const S: ShortString): string;
begin
  Result := UTF8ToUnicodeString(S);
end;

function UTF8ToString(const S: PAnsiChar): string;
begin
  Result := UTF8ToUnicodeString(S);
end;

function UTF8ToString(const S: array of AnsiChar): string;
begin
  Result := UTF8ToUnicodeString(@S[0]);
end;

function AnsiToUtf8(const S: string): RawByteString;
begin
  Result := Utf8Encode(S);
end;

function Utf8ToAnsi(const S: RawByteString): string;
begin
  Result := string(Utf8ToUnicodeString(S));
end;

{$IFDEF LINUX}


function GetCPUType: Integer;
asm
      PUSH      EBX
    // this code assumes ESP is 4 byte aligned
    // test for 80386:  see if bit #18 of EFLAGS (Alignment fault) can be toggled
      PUSHFD
      POP       EAX
      MOV       ECX, EAX
      XOR       EAX, $40000   // flip AC bit in EFLAGS
      PUSH      EAX
      POPFD
      PUSHFD
      POP       EAX
      XOR       EAX, ECX      // zero = 80386 CPU (can't toggle AC bit)
      MOV       EAX, CPUi386
      JZ        @@Exit
      PUSH      ECX
      POPFD                    // restore original flags before next test

      // test for 80486:  see if bit #21 of EFLAGS (CPUID supported) can be toggled
      MOV       EAX, ECX        // get original EFLAGS
      XOR       EAX, $200000    // flip CPUID bit in EFLAGS
      PUSH      EAX
      POPFD
      PUSHFD
      POP       EAX
      XOR       EAX, ECX    // zero = 80486 (can't toggle EFLAGS bit #21)
      MOV       EAX, CPUi486
      JZ        @@Exit

      // Use CPUID instruction to get CPU family
      XOR       EAX, EAX
      CPUID
      CMP       EAX, 1
      JL        @@Exit          // unknown processor response: report as 486
      XOR       EAX, EAX
      INC       EAX       // we only care about info level 1
      CPUID
      AND       EAX, $F00
      SHR       EAX, 8
      // Test8086 values are one less than the CPU model number, for historical reasons
      DEC       EAX

@@Exit:
      POP       EBX
end;

{$ENDIF LINUX}

{$IFDEF LINUX_OLD_RESOURCES }
const
  sResSymExport = '@Sysinit@ResSym';
  sResStrExport = '@Sysinit@ResStr';
  sResHashExport = '@Sysinit@ResHash';

type
  TElf32Sym = record
    Name: Cardinal;
    Value: Pointer;
    Size: Cardinal;
    Info: Byte;
    Other: Byte;
    Section: Word;
  end;
  PElf32Sym = ^TElf32Sym;

  TElfSymTab = array [0..0] of TElf32Sym;
  PElfSymTab = ^TElfSymTab;

  TElfWordTab = array [0..2] of Cardinal;
  PElfWordTab = ^TElfWordTab;


{ If Name encodes a numeric identifier, return it, else return -1.  }
function NameToId(Name: PChar): Longint;
var digit: Longint;
begin
  if LongInt(NativeInt(Name)) and $ffff0000 = 0 then
  begin
    Result := LongInt(NativeInt(Name)) and $ffff;
  end
  else if Name^ = '#' then
  begin
    Result := 0;
    inc (Name);
    while (Ord(Name^) <> 0) do
    begin
      digit := Ord(Name^) - Ord('0');
      if (LongWord(digit) > 9) then
      begin
        Result := -1;
        exit;
      end;
      Result := Result * 10 + digit;
      inc (Name);
    end;
  end
  else
    Result := -1;
end;


// Return ELF hash value for NAME converted to lower case.
function ElfHashLowercase(Name: PChar): Cardinal;
var
  g: Cardinal;
  c: Char;
begin
  Result := 0;
  while name^ <> #0 do
  begin
    c := name^;
    c := tolower(c);
    Result := (Result shl 4) + Ord(c);
    g := Result and $f0000000;
    Result := (Result xor (g shr 24)) and not g;
    Inc(name);
  end;
end;

type
  PFindResourceCache = ^TFindResourceCache;
  TFindResourceCache = record
    ModuleHandle: HMODULE;
    Version: Cardinal;
    SymbolTable: PElfSymTab;
    StringTable: PChar;
    HashTable: PElfWordTab;
    BaseAddress: Pointer;
  end;


{$IFDEF CPUX64}
var
{$ELSE}
threadvar
{$ENDIF}
  FindResourceCache: TFindResourceCache;

function GetResourceCache(ModuleHandle: HMODULE): PFindResourceCache;
var
  Info: dl_info;
begin
  Result := @FindResourceCache;
  if (ModuleHandle <> Result^.ModuleHandle) or (ModuleCacheVersion <> Result^.Version) then
  begin
    Result^.SymbolTable := dlsym(ModuleHandle, sResSymExport);
    Result^.StringTable := dlsym(ModuleHandle, sResStrExport);
    Result^.HashTable := dlsym(ModuleHandle, sResHashExport);
    Result^.ModuleHandle := ModuleHandle;
    if (dladdr(Result^.HashTable, Info) = 0) or (Info.dli_fbase = ExeBaseAddress) then
      Result^.BaseAddress := nil   // if it's not in a library, assume the exe
    else
      Result^.BaseAddress := Info.dli_fbase;
    Result^.Version := ModuleCacheVersion;
  end;
end;

function FindResource(ModuleHandle: HMODULE; ResourceName: PChar; ResourceType: PChar): TResourceHandle;
var
  P: PFindResourceCache;
  nid, tid: Longint;
  ucs2_key: array [0..2] of WideChar;
  key: array [0..127] of Char;
  len: Integer;
  pc: PChar;
  ch: Char;
  nbucket: Cardinal;
  bucket, chain: PElfWordTab;
  syndx: Cardinal;
begin
  Result := 0;
  if ResourceName = nil then Exit;
  P := GetResourceCache(ModuleHandle);

  tid := NameToId (ResourceType);
  if tid = -1 then Exit;  { not supported (yet?) }

  { This code must match util-common/elfres.c }
  nid := NameToId (ResourceName);
  if nid = -1 then
  begin
    ucs2_key[0] := WideChar(2*tid+2);
    ucs2_key[1] := WideChar(0);
    len := UnicodeToUtf8 (key, ucs2_key, SizeOf (key)) - 1;
    pc := key+len;
    while Ord(ResourceName^) <> 0 do
    begin
      ch := ResourceName^;
      if Ord(ch) > 127 then exit; { insist on 7bit ASCII for now }
      if ('A' <= ch) and (ch <= 'Z') then Inc(ch, Ord('a') - Ord('A'));
      pc^ := ch;
      inc (pc);
      if pc = key + SizeOf(key) then exit;
      inc (ResourceName);
    end;
    pc^ := Char(0);
  end
  else
  begin
    ucs2_key[0] := WideChar(2*tid+1);
    ucs2_key[1] := WideChar(nid);
    ucs2_key[2] := WideChar(0);
    UnicodeToUtf8 (key, ucs2_key, SizeOf (key));
  end;

  with P^ do
  begin
    nbucket := HashTable[0];
  //  nsym := HashTable[1];
    bucket := @HashTable[2];
    chain := @HashTable[2+nbucket];

    syndx := bucket[ElfHashLowercase(key) mod nbucket];
    while (syndx <> 0)
      and (strcasecmp(key, @StringTable[SymbolTable[syndx].Name]) <> 0) do
      syndx := chain[syndx];

    if syndx = 0 then
      Result := 0
    else
      Result := TResourceHandle(@SymbolTable[syndx]);
  end;
end;

function LoadResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): HGLOBAL;
var
  P: PFindResourceCache;
begin
  if ResHandle <> 0 then
  begin
    P := GetResourceCache(ModuleHandle);
    Result := HGLOBAL(PElf32Sym(ResHandle)^.Value);
    Inc(NativeUInt(Result), NativeUInt(P^.BaseAddress));
  end
  else
    Result := 0;
end;

function SizeofResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): Integer;
begin
  if ResHandle <> 0 then
    Result := PElf32Sym(ResHandle)^.Size
  else
    Result := 0;
end;

function LockResource(ResData: HGLOBAL): Pointer;
begin
  Result := Pointer(ResData);
end;

function UnlockResource(ResData: HGLOBAL): LongBool;
begin
  Result := False;
end;

function FreeResource(ResData: HGLOBAL): LongBool;
begin
  Result := True;
end;
{$ENDIF LINUX_OLD_RESOURCES}

{$IFDEF POSIX}
const
  sResSymExport = 'SysinitResSym';

type
  TResourceHeader = record
    SymtabOffset: Cardinal;
    SymbolCount: Integer;
    HashtableOffset: Cardinal;
  end;
  PResourceHeader = ^TResourceHeader;
  TResourceSym = record
    Name:  Cardinal;
    Data:  Cardinal;
    Size:  Cardinal;
    Chain: Cardinal;            // link to next hash entry, or -1 for end
  end;
  PResourceSym = ^TResourceSym;

  Hashtable = record
    Size: Cardinal;
    Entries: array [0..0] of Cardinal;
  end;
  PHashtable = ^Hashtable;

  TResourceSymTab = array [0..0] of TResourceSym;
  PResourceSymTab = ^TResourceSymTab;

{ If Name encodes a numeric identifier, return it, else return -1.  }
function NameToId(Name: PChar): Longint;
var digit: Longint;
begin
  if LongInt(NativeInt(Name)) and $ffff0000 = 0 then
  begin
    Result := LongInt(NativeInt(Name)) and $ffff;
  end
  else if Name^ = '#' then
  begin
    Result := 0;
    inc (Name);
    while (Ord(Name^) <> 0) do
    begin
      digit := Ord(Name^) - Ord('0');
      if (LongWord(digit) > 9) then
      begin
        Result := -1;
        exit;
      end;
      Result := Result * 10 + digit;
      inc (Name);
    end;
  end
  else
    Result := -1;
end;


// Return ELF hash value for NAME converted to lower case.
function ElfHashLowercase(Name: PAnsiChar): Cardinal;
var
  g: Cardinal;
  c: AnsiChar;
begin
  Result := 0;
  while name^ <> #0 do
  begin
    c := name^;
    c := AnsiChar(towlower(UCS4Char(c)));
    Result := (Result shl 4) + Ord(c);
    g := Result and $f0000000;
    Result := (Result xor (g shr 24)) and not g;
    Inc(name);
  end;
end;

type
  PFindResourceCache = ^TFindResourceCache;
  TFindResourceCache = record
    ModuleHandle: HMODULE;
    Version: Cardinal;
    ResourceHeader: PResourceHeader;
    SymbolTable: PResourceSymTab;
    StringTable: PAnsiChar;
    BaseAddress: Pointer;
    Hashtable: PHashtable;
  end;


{$IFDEF CPUX64}
var
{$ELSE}
threadvar
{$ENDIF}
  FindResourceCache: TFindResourceCache;

function GetResourceCache(ModuleHandle: HMODULE): PFindResourceCache;
var
  info: dl_info;
begin
  Result := @FindResourceCache;
  if (ModuleHandle <> Result^.ModuleHandle) or (ModuleCacheVersion <> Result^.Version) then
  begin
    Result^.ResourceHeader := dlsym(ModuleHandle, sResSymExport);
    Result^.SymbolTable := PResourceSymTab((NativeInt(Result^.ResourceHeader) +
                                               SizeOf(TResourceHeader)));
    Result^.StringTable := PAnsiChar((NativeInt(Result^.ResourceHeader) +
                                         SizeOf(TResourceHeader) +
                                         SizeOf(TResourceSym) * Result^.ResourceHeader^.SymbolCount));
    Result^.ModuleHandle := ModuleHandle;
    if (dladdr(NativeUInt(Result^.ResourceHeader), Info) = 0) or (Info.dli_fbase = ExeBaseAddress) then
      Result^.BaseAddress := nil   // if it's not in a library, assume the exe
    else
      Result^.BaseAddress := Info.dli_fbase;
    Result^.Version := ModuleCacheVersion;
    Result^.Hashtable := PHashtable( NativeUInt(Result^.ResourceHeader)
                                        +  Result^.ResourceHeader^.HashtableOffset);
  end;
end;

// UCS4cToUTF8c matches code in util-common, and must be kept in sync.
function UCS4cToUTF8c(Dest: PAnsiChar; Src: UInt32): Integer;
begin
  if ((Src and not $7F) = 0) then begin
    Dest^ := AnsiChar(Src);
    Result := 1;
  end
  else if ((Src and not $7FF) = 0) then begin
    Dest^ := AnsiChar($C0 or (Src shr 6));
    Inc(Dest);
    Dest^ := AnsiChar($80 or (Src and $3F));
    Result := 2;
  end
  else if ((Src and not $FFFF) = 0) then begin
    Dest^ := AnsiChar($E0 or (Src shr 12));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 6) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or (Src and $3F));
    Result := 3;
  end
  else if ((Src and not $1FFFFF) = 0) then begin
    Dest^ := AnsiChar($F0 or (Src shr 18));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 12) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 6) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or (Src and $3F));
    Result := 4;
  end
  else if ((Src and not $3FFFFFF) = 0) then begin
    Dest^ := AnsiChar($F8 or (Src shr 24));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 18) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 12) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 16) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or (Src and $3F));
    Result := 5;
  end
  else if ((Src and not $7FFFFFFFF) = 0) then begin
    Dest^ := AnsiChar($F0 or (Src shr 30));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 24) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 18) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 12) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or ((Src shr 16) and $3F));
    Inc(Dest);
    Dest^ := AnsiChar($80 or (Src and $3F));
    Result := 6;
  end
  else
     Result := -1;
end;

function FindResource(ModuleHandle: HMODULE; ResourceName: PChar; ResourceType: PChar): TResourceHandle;
var
  ResCacheEntry: PFindResourceCache;
  nid, tid: Longint;
  ucs2_key: array [0..2] of WideChar;
  key: array [0..127] of AnsiChar;
  len: Integer;
  pc: PAnsiChar;
  ch: AnsiChar;
  symName: PAnsiChar;
  utf8ResName: PAnsiChar;
  I: Integer;
  syndx: Cardinal;
begin
  Result := 0;
  if ResourceName = nil then Exit;
  ResCacheEntry := GetResourceCache(ModuleHandle);

  tid := NameToId (ResourceType);
  if tid = -1 then Exit;  { not supported (yet?) }

  { This code must match util-common/elfres.c }
  nid := NameToId (ResourceName);
  if nid = -1 then
  begin
    ucs2_key[0] := WideChar(2*tid+2);
    ucs2_key[1] := WideChar(0);
    len := UCS4cToUTF8c(key, 2*tid+2);
    pc := key+len;
    utf8ResName := PAnsiChar(AnsiString(ResourceName));
    while Ord(utf8ResName^) <> 0 do
    begin
      ch := utf8ResName^;
      if Ord(ch) > 127 then exit; { insist on 7bit ASCII for now }
      if ('A' <= ch) and (ch <= 'Z') then Inc(ch, Ord('a') - Ord('A'));
      pc^ := ch;
      inc (pc);
      if pc = key + SizeOf(key) then exit;
      inc (utf8ResName);
    end;
    pc^ := Char(0);
  end
  else
  begin
    len := UCS4cToUTF8c(key, 2*tid+1);
    len := len + UCS4cToUTF8c(key + len, nid);
    key[len] := #0;
  end;

  with ResCacheEntry^ do
  begin
    syndx := ElfHashLowercase(key) mod Hashtable^.Size;
    I := Hashtable^.Entries[syndx];
    while I <> -1 do
    begin
      symName := StringTable + SymbolTable[I].Name;
      if (strcasecmp(key, symName) = 0) then
      begin
        Result := TResourceHandle(@SymbolTable[I]);
        Exit;
      end
      else
      begin
        I := SymbolTable[I].Chain;
      end;
    end;
  end;

end;

function LoadResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): HGLOBAL;
var
  ResCacheEntry: PFindResourceCache;
begin
  if ResHandle <> 0 then
  begin
    ResCacheEntry := GetResourceCache(ModuleHandle);
    Result := HGLOBAL(PResourceSym(ResHandle)^.Data + HGLOBAL(ResCacheEntry^.ResourceHeader));
  end
  else
    Result := 0;
end;

function SizeofResource(ModuleHandle: HMODULE; ResHandle: TResourceHandle): Integer;
begin
  if ResHandle <> 0 then
    Result := PResourceSym(ResHandle)^.Size
  else
    Result := 0;
end;

function LockResource(ResData: HGLOBAL): Pointer;
begin
  Result := Pointer(ResData);
end;

function UnlockResource(ResData: HGLOBAL): LongBool;
begin
  Result := False;
end;

function FreeResource(ResData: HGLOBAL): LongBool;
begin
  Result := True;
end;
{$ENDIF POSIX}

{ ResString support function }

{$IFDEF MSWINDOWS}
function LoadResString(ResStringRec: PResStringRec): string;
var
  Buffer: array [0..4095] of Char;
begin
  if ResStringRec = nil then Exit;
  if ResStringRec.Identifier < 64*1024 then
    SetString(Result, Buffer,
      LoadString(FindResourceHInstance(ResStringRec.Module^),
        ResStringRec.Identifier, Buffer, Length(Buffer)))
  else
    Result := PChar(ResStringRec.Identifier);
end;
{$ENDIF}

{$IFDEF POSIX}

const
  ResStringTableLen = 16;

type
  ResStringTable = array [0..ResStringTableLen-1] of LongWord;

function LoadResString(ResStringRec: PResStringRec): string;
var
  Handle: TResourceHandle;
  Tab: ^ResStringTable;
  ResMod: HMODULE;
  blk: Integer;
begin
  if ResStringRec = nil then Exit;
  ResMod := FindResourceHInstance(ResStringRec^.Module^);
  //P := GetResourceCache(ModuleHandle);
  blk := ResStringRec^.Identifier div ResStringTableLen;
  if blk = 0 then    // can't pass 'nil' to FindResource. use alternate scheme for 0 instead
    Handle := FindResource(ResMod, '#0', PChar(6)) // RT_STRING
  else
    Handle := FindResource(ResMod,
//         PChar(ResStringRec^.Identifier),
       PChar(blk),
//       PChar(((blk + 1) shl 16) or $FFFF),
       PChar(6));   // RT_STRING
  Tab := Pointer(LoadResource(ResMod, Handle));
  if Tab = nil then
    Result := ''
  else
  begin
    Result := PWideChar(PAnsiChar(Tab) + Tab[ResStringRec^.Identifier mod ResStringTableLen]);
  end;
end;
{$ENDIF POSIX}

{$IFDEF LINUX}
{ The Win32 program loader sets up the first 64k of process address space
  with no read or write access, to help detect use of invalid pointers
  (whose integer value is 0..64k).  Linux doesn't do this.  Mac OS/X
  does allow this, and the linker will ensure that this is the case by
  reserving 64k at the start of the image.

  Parts of the Delphi RTL and IDE design environment
  rely on the notion that pointer values in the [0..64k] range are
  invalid pointers.  To accomodate this in Linux, we reserve the range
  at startup.  If the range is already allocated, we keep going anyway. }

var
  ZeroPageReserved: Boolean = False;

procedure ReserveZeroPage;
const
  PROT_NONE = 0;
  MAP_PRIVATE   = $02;
  MAP_FIXED     = $10;
  MAP_ANONYMOUS = $20;
var
  P: Pointer;
begin
  if IsLibrary then Exit;  // page reserve is app's job, not .so's

  if not ZeroPageReserved then
  begin
    P := mmap(nil, High(Word), PROT_NONE,
      MAP_ANONYMOUS or MAP_PRIVATE or MAP_FIXED, 0, 0);
    ZeroPageReserved := P = nil;
    if (NativeInt(P) <> -1) and (P <> nil) then  // we didn't get it
      munmap(P, High(Word));
  end;
end;

procedure ReleaseZeroPage;
begin
  if ZeroPageReserved then
  begin
    munmap(nil, High(Word) - 4096);
    ZeroPageReserved := False;
  end;
end;
{$ENDIF}

var
  xxNull: UCS4Char = 0;
  xxPNull: PUCS4Char = @xxNull;

function PUCS4Chars(const S: UCS4String): PUCS4Char;
begin
  if Length(S) > 0 then
    Result := @S[0]
  else
    Result := xxPNull;
end;

function WideStringToUCS4String(const S: WideString): UCS4String;
var
  I: Integer;
  CharCount: Integer;
begin
  CharCount := 0;
  SetLength(Result, Length(S) + 1);
  I := 0;

  while I < Length(S) do
  begin

    if ((S[I + 1] >= #$D800) and (S[I + 1] <= #$DFFF)) and (I + 1 < Length(S)) then
    begin
      Result[CharCount] := UCS4Char((Cardinal(S[I + 1]) and $000003FF) shl 10 or (Cardinal(S[I + 2]) and $000003FF) + $00010000);
      Inc(I);
    end
    else
      Result[CharCount] := UCS4Char(S[I + 1]);

    Inc(CharCount);
    Inc(I);
  end;
  Result[CharCount] := 0;
  SetLength(Result, CharCount + 1);
end;

function UCS4StringToWideString(const S: UCS4String): WideString;
var
  I: Integer;
  CharCount: Integer;
begin
  SetLength(Result, Length(S) * 2 - 1); //Maximum possible number of characters
  CharCount := 0;

  I := 0;
  while I < Length(S) - 1 do
  begin
    if S[I] >= $10000 then
    begin
      Inc(CharCount);
      Result[CharCount] := WideChar((((S[I] - $00010000) shr 10) and $000003FF) or $D800);
      Inc(CharCount);
      Result[CharCount] := WideChar(((S[I] - $00010000) and $000003FF)or $DC00);
    end
    else
    begin
      Inc(CharCount);
      Result[CharCount] := WideChar(S[I]);
    end;

    Inc(I);
  end;

  SetLength(Result, CharCount);
end;

{$IFDEF POSIX}
type
  TCodePageMapEntry = record
    LocaleName: string;
    CodePage: Cardinal;
  end;


const
  { Predefined set of Name <=> CP mappings for POSIX }
  CodePageMapA: array[0..18] of TCodePageMapEntry = (
    //(LocaleName: 'am_et'; CodePage: 0),
    (LocaleName: 'ar_ae'; CodePage: 1256),
    (LocaleName: 'ar_bh'; CodePage: 1256),
    (LocaleName: 'ar_dz'; CodePage: 1256),
    (LocaleName: 'ar_eg'; CodePage: 1256),
    (LocaleName: 'ar_iq'; CodePage: 1256),
    (LocaleName: 'ar_jo'; CodePage: 1256),
    (LocaleName: 'ar_kw'; CodePage: 1256),
    (LocaleName: 'ar_lb'; CodePage: 1256),
    (LocaleName: 'ar_ly'; CodePage: 1256),
    (LocaleName: 'ar_ma'; CodePage: 1256),
    (LocaleName: 'ar_om'; CodePage: 1256),
    (LocaleName: 'ar_qa'; CodePage: 1256),
    (LocaleName: 'ar_sa'; CodePage: 1256),
    (LocaleName: 'ar_sd'; CodePage: 1256),
    (LocaleName: 'ar_sy'; CodePage: 1256),
    (LocaleName: 'ar_tn'; CodePage: 1256),
    (LocaleName: 'ar_ye'; CodePage: 1256),
    //(LocaleName: 'as_in'; CodePage: 0),
    (LocaleName: 'az-cyrl_az'; CodePage: 1251),
    (LocaleName: 'az-latn_az'; CodePage: 1254));

  CodePageMapBC: array[0..2] of TCodePageMapEntry = (
    (LocaleName: 'be_by'; CodePage: 1251),
    (LocaleName: 'bg_bg'; CodePage: 1251),
    //(LocaleName: 'bn_bd'; CodePage: 0),
    //(LocaleName: 'bn_in'; CodePage: 0),
    (LocaleName: 'cs_cz'; CodePage: 1250));

  CodePageMapEF: array[0..2] of TCodePageMapEntry = (
    //(LocaleName: 'el_cy'; CodePage: 0),
    (LocaleName: 'el_gr'; CodePage: 1253),
    (LocaleName: 'et_ee'; CodePage: 1257),
    //(LocaleName: 'fa_af'; CodePage: 0),
    (LocaleName: 'fa_ir'; CodePage: 1256));

  CodePageMapGH: array[0..2] of TCodePageMapEntry = (
    //(LocaleName: 'gu_in'; CodePage: 0),
    (LocaleName: 'he_il'; CodePage: 1255),
    //(LocaleName: 'hi_in'; CodePage: 0),
    (LocaleName: 'hr_hr'; CodePage: 1250),
    (LocaleName: 'hu_hu'; CodePage: 1250));
    //(LocaleName: 'hy_am'; CodePage: 0),

  CodePageMapJK: array[0..2] of TCodePageMapEntry = (
    //(LocaleName: 'ii_cn'; CodePage: 0),
    (LocaleName: 'ja_jp'; CodePage: 932),
    //(LocaleName: 'ka_ge'; CodePage: 0),
    (LocaleName: 'kk_kz'; CodePage: 1251),
    //(LocaleName: 'km_kh'; CodePage: 0),
    //(LocaleName: 'kn_in'; CodePage: 0),
    (LocaleName: 'ko_kr'; CodePage: 949));
    //(LocaleName: 'kok_in'; CodePage: 0),

  CodePageMapLM: array[0..2] of TCodePageMapEntry = (
    (LocaleName: 'lt_lt'; CodePage: 1257),
    (LocaleName: 'lv_lv'; CodePage: 1257),
    (LocaleName: 'mk_mk'; CodePage: 1251));
    //(LocaleName: 'ml_in'; CodePage: 0),
    //(LocaleName: 'mr_in'; CodePage: 0),
    //(LocaleName: 'mt_mt'; CodePage: 0),

  CodePageMapNP: array[0..1] of TCodePageMapEntry = (
    //(LocaleName: 'ne_in'; CodePage: 0),
    //(LocaleName: 'ne_np'; CodePage: 0),
    //(LocaleName: 'om_et'; CodePage: 0),
    //(LocaleName: 'om_ke'; CodePage: 0),
    //(LocaleName: 'or_in'; CodePage: 0),
    (LocaleName: 'pa-arab_pk'; CodePage: 1256),
    //(LocaleName: 'pa_in'; CodePage: 0),
    (LocaleName: 'pl_pl'; CodePage: 1250));
    //(LocaleName: 'ps_af'; CodePage: 0),

  CodePageMapR: array[0..1] of TCodePageMapEntry = (
    (LocaleName: 'ro_ro'; CodePage: 1250),
    (LocaleName: 'ru_ru'; CodePage: 1251));
    //(LocaleName: 'si_lk'; CodePage: 0),

  CodePageMapS: array[0..8] of TCodePageMapEntry = (
    (LocaleName: 'sk_sk'; CodePage: 1250),
    (LocaleName: 'sl_si'; CodePage: 1250),
    //(LocaleName: 'so_dj'; CodePage: 0),
    //(LocaleName: 'so_et'; CodePage: 0),
    //(LocaleName: 'so_ke'; CodePage: 0),
    //(LocaleName: 'so_so'; CodePage: 0),
    (LocaleName: 'sq_al'; CodePage: 1250),
    (LocaleName: 'sr-cyrl_ba'; CodePage: 1251),
    (LocaleName: 'sr-cyrl_me'; CodePage: 1251),
    (LocaleName: 'sr-cyrl_rs'; CodePage: 1251),
    (LocaleName: 'sr-latn_ba'; CodePage: 1250),
    (LocaleName: 'sr-latn_me'; CodePage: 1250),
    (LocaleName: 'sr-latn_rs'; CodePage: 1250));

  CodePageMapTU: array[0..6] of TCodePageMapEntry = (
    //(LocaleName: 'ta_in'; CodePage: 0),
    //(LocaleName: 'te_in'; CodePage: 0),
    (LocaleName: 'th_th'; CodePage: 874),
    //(LocaleName: 'ti_er'; CodePage: 0),
    //(LocaleName: 'ti_et'; CodePage: 0),
    (LocaleName: 'tr_tr'; CodePage: 1254),
    (LocaleName: 'uk_ua'; CodePage: 1251),
    //(LocaleName: 'ur_in'; CodePage: 0),
    (LocaleName: 'ur_pk'; CodePage: 1256),
    (LocaleName: 'uz-arab_af'; CodePage: 1256),
    (LocaleName: 'uz-cyrl_uz'; CodePage: 1251),
    (LocaleName: 'uz-latn_uz'; CodePage: 1254));

  CodePageMapVZ: array[0..7] of TCodePageMapEntry = (
    (LocaleName: 'vi_vn'; CodePage: 1258),
    (LocaleName: 'zh_cn'; CodePage: 936),
    (LocaleName: 'zh_hk'; CodePage: 950),
    (LocaleName: 'zh-hans_hk'; CodePage: 936),
    (LocaleName: 'zh_mo'; CodePage: 950),
    (LocaleName: 'zh-hans_mo'; CodePage: 936),
    (LocaleName: 'zh_sg'; CodePage: 936),
    (LocaleName: 'zh_tw'; CodePage: 950));

function GetPosixLocaleName: string;
{$IFDEF MACOS}

  function StringRefToString(StringRef: CFStringRef): string;
  var
    Range: CFRange;
  begin
    Range.location := 0;
    Range.length := CFStringGetLength(StringRef);
    if Range.length > 0 then
    begin
      SetLength(Result, Range.length);
      CFStringGetCharacters(StringRef, Range, @Result[1]);
    end
    else
      Result := '';
  end;

var
  Locale: CFLocaleRef;
begin
  Locale := CFLocaleCopyCurrent;
  try
    Result := StringRefToString(CFLocaleGetIdentifier(Locale));
  finally
    CFRelease(Locale);
  end;
end;
{$ELSE}
var
  P: Integer;
begin
  Result := string(getenv(PAnsiChar('LANG'))); // do not localize
  P := Pos('.', Result);
  if P <> 0 then
    SetLength(Result, P - 1);
end;
{$ENDIF MACOS}

function GetACP: Cardinal;

  function FindCodePage(const Name: string; const Map: array of TCodePageMapEntry;
    var CodePage: Cardinal): Boolean;
  var
    I: Integer;
  begin
    for I := Low(Map) to High(Map) do
      if Map[I].LocaleName = Name then
      begin
        CodePage := Map[I].CodePage;
        Exit(True);
      end;
    Result := False;
  end;

var
  I: Integer;
  LName: string;
  LCodePage: Cardinal;
begin
  LName := GetPosixLocaleName;
  for I := 1 to Length(LName) do
    if LName[I] in ['A'..'Z'] then         // do not localize
      Inc(LName[I], Ord('a') - Ord('A'));  // do not localize


  Result := 1252; // Default codepage
  if Length(LName) > 0 then
    case LName[1] of
      'a':
        if FindCodePage(LName, CodePageMapA, Result) then
          Result := LCodePage;
      'b','c':
        if FindCodePage(LName, CodePageMapBC, LCodePage) then
          Result := LCodePage;
      'e','f':
        if FindCodePage(LName, CodePageMapEF, LCodePage) then
          Result := LCodePage;
      'g','h':
        if FindCodePage(LName, CodePageMapGH, LCodePage) then
          Result := LCodePage;
      'j','k':
        if FindCodePage(LName, CodePageMapJK, LCodePage) then
          Result := LCodePage;
      'l','m':
        if FindCodePage(LName, CodePageMapLM, LCodePage) then
          Result := LCodePage;
      'n','p':
        if FindCodePage(LName, CodePageMapNP, LCodePage) then
          Result := LCodePage;
      'r':
        if FindCodePage(LName, CodePageMapR, LCodePage) then
          Result := LCodePage;
      's':
        if FindCodePage(LName, CodePageMapS, LCodePage) then
          Result := LCodePage;
      't','u':
        if FindCodePage(LName, CodePageMapTU, LCodePage) then
          Result := LCodePage;
      'v','z':
        if FindCodePage(LName, CodePageMapVZ, LCodePage) then
          Result := LCodePage;
    end;
end;
{$ENDIF POSIX}



{$IFDEF POSIX}
function LocaleNameFromCodePage(CodePage: Integer): AnsiString;
begin
  if CodePage = CP_ACP then
    CodePage := GetACP;
  case CodePage of
    // Special cases
    10000:   Result := 'MACROMAN';         // do not localize
    10004:   Result := 'MACARABIC';        // do not localize
    10005:   Result := 'MACHEBREW';        // do not localize
    10006:   Result := 'MACGREEK';         // do not localize
    10007:   Result := 'MACCYRILLIC';      // do not localize
    10010:   Result := 'MACROMANIA';       // do not localize
    10017:   Result := 'MACUKRAINE';       // do not localize
    10021:   Result := 'MACTHAI';          // do not localize
    10029:   Result := 'MACCENTRALEUROPE'; // do not localize
    10079:   Result := 'MACICELAND';       // do not localize
    10081:   Result := 'MACTURKISH';       // do not localize
    10082:   Result := 'MACCROATIAN';      // do not localize
    12000:   Result := 'UTF-32LE';         // do not localize
    12001:   Result := 'UTF-32BE';         // do not localize
    20127:   Result := 'ASCII';            // do not localize
    20866:   Result := 'KOI8-R';           // do not localize
    20932:   Result := 'EUC-JP';           // do not localize
    20936:   Result := 'GB2312';           // do not localize
    21866:   Result := 'KOI8-U';           // do not localize
    28591:   Result := 'ISO-8859-1';       // do not localize
    28592:   Result := 'ISO-8859-2';       // do not localize
    28593:   Result := 'ISO-8859-3';       // do not localize
    28594:   Result := 'ISO-8859-4';       // do not localize
    28595:   Result := 'ISO-8859-5';       // do not localize
    28596:   Result := 'ISO-8859-6';       // do not localize
    28597:   Result := 'ISO-8859-7';       // do not localize
    28598:   Result := 'ISO-8859-8';       // do not localize
    28599:   Result := 'ISO-8859-9';       // do not localize
    28600:   Result := 'ISO-8859-10';      // do not localize
    28601:   Result := 'ISO-8859-11';      // do not localize
    28603:   Result := 'ISO-8859-13';      // do not localize
    28604:   Result := 'ISO-8859-14';      // do not localize
    28605:   Result := 'ISO-8859-15';      // do not localize
    28606:   Result := 'ISO-8859-16';      // do not localize
    50221:   Result := 'ISO-2022-JP';      // do not localize
    50225:   Result := 'ISO-2022-KR';      // do not localize
    50227:   Result := 'ISO-2022-CN';      // do not localize
    51932:   Result := 'EUC-JP';           // do not localize
    51936:   Result := 'GB2312';           // do not localize
    51949:   Result := 'EUC-KR';           // do not localize
    51950:   Result := 'EUC-TW';           // do not localize
    52936:   Result := 'HZ-GB-2312';       // do not localize
    54936:   Result := 'GB18030';          // do not localize
    CP_UTF7: Result := 'UTF-7';            // do not localize
    CP_UTF8: Result := 'UTF-8';            // do not localize
  else
    Str(CodePage, Result);
    Result := 'cp' + Result;  // do not localize
  end;
end;
{$ENDIF POSIX}


function LocaleCharsFromUnicode(CodePage, Flags: Cardinal;
  UnicodeStr: PWideChar; UnicodeStrLen: Integer; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; DefaultChar: PAnsiChar; UsedDefaultChar: PLongBool): Integer; overload;
{$IFDEF MSWINDOWS}
begin
  Result := WideCharToMultiByte(CodePage, Flags, UnicodeStr, UnicodeStrLen, LocaleStr,
    LocaleStrLen, DefaultChar, UsedDefaultChar);
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  Result := LocaleCharsFromUnicode(LocaleNameFromCodePage(CodePage), Flags,
    UnicodeStr, UnicodeStrLen, LocaleStr, LocaleStrLen, DefaultChar, UsedDefaultChar);
end;
{$ENDIF POSIX}


function UnicodeFromLocaleChars(CodePage, Flags: Cardinal; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; UnicodeStr: PWideChar; UnicodeStrLen: Integer): Integer; overload;
{$IFDEF MSWINDOWS}
begin
  Result := MultiByteToWideChar(CodePage, Flags, LocaleStr, LocaleStrLen,
    UnicodeStr, UnicodeStrLen);
end;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
begin
  Result := UnicodeFromLocaleChars(LocaleNameFromCodePage(CodePage), Flags,
    LocaleStr, LocaleStrLen, UnicodeStr, UnicodeStrLen);
end;
{$ENDIF POSIX}

{$IFDEF POSIX}
const
  MB_ERR_INVALID_CHARS = 8;

function LocaleCharsFromUnicode(const LocaleName: AnsiString; Flags: Cardinal;
  UnicodeStr: PWideChar; UnicodeStrLen: Integer; LocaleStr: PAnsiChar;
  LocaleStrLen: Integer; DefaultChar: PAnsiChar; UsedDefaultChar: PLongBool): Integer; overload;
var
  LContext: iconv_t;
  LSourcePtr, LDestPtr: Pointer;
  LCalcSizeBuf: array of AnsiChar;
  LDestLen, LSourceLen, LSize: Integer;
begin
  Result := 0;
  // DefaultChar and UsedDefaultChar are not valid with UTF-7 or UTF-8
  if ((DefaultChar <> nil) or (UsedDefaultChar <> nil)) and
     ((LocaleName = 'UTF-8') or (LocaleName = 'UTF-7')) then // do not localize
    Exit(0);

  // When UnicodeStrLen is -1 treat UnicodeStr as a null terminated string
  if UnicodeStrLen = -1 then
    UnicodeStrLen := Length(UnicodeStr) + 1; // include terminating null

  LContext := iconv_open(PAnsiChar(LocaleName), 'UTF-16LE'); // do not localize
  if LContext <> iconv_t(-1) then
  try
  // LocaleStrLen = 0 is a request to calculate the required buffer size,
  // use a local destination buffer for the conversion.
  if LocaleStrLen = 0 then
  begin
    SetLength(LCalcSizeBuf, 1024);
    LDestLen := Length(LCalcSizeBuf);
    LDestPtr := Pointer(@LCalcSizeBuf[0]);
  end
  else
  begin
    LDestLen := LocaleStrLen;
    LDestPtr := Pointer(LocaleStr);
  end;

  LSourceLen := UnicodeStrLen * SizeOf(WideChar);
  LSourcePtr := Pointer(UnicodeStr);

  LSize := 0;
  if UsedDefaultChar <> nil then
    UsedDefaultChar^ := False;

  while True do
  begin
      Result := iconv(LContext, @LSourcePtr, @LSourceLen, @LDestPtr, @LDestLen);
    if Result <> -1 then
      Break
    else
    begin
      case GetLastError of
        E2BIG: // Insufficient destination buffer
          if LocaleStrLen = 0 then
          begin
            // Save converted buffer size and reset to beginning of local buffer
            Inc(LSize, Length(LCalcSizeBuf) - LDestLen);
            LDestPtr := Pointer(@LCalcSizeBuf[0]);
            LDestLen := Length(LCalcSizeBuf);
          end
          else
          begin
            LDestLen := LocaleStrLen; // Return a length of 0
            Break;
          end;
        EILSEQ: // Invalid character for destination character set
          begin
            // Increment pointers and insert '?' (or the DefaultChar
            // if specified) into the destination string.
            Inc(NativeInt(LSourcePtr), SizeOf(WideChar));
            Dec(LSourceLen, SizeOf(WideChar));
            if LocaleStrLen <> 0 then
            begin
              if DefaultChar = nil then
                PAnsiChar(LDestPtr)^ := AnsiChar('?') // do not localize
              else
                PAnsiChar(LDestPtr)^ := DefaultChar^;
              if UsedDefaultChar <> nil then
                UsedDefaultChar^ := True;
            end;
            Inc(NativeInt(LDestPtr));
            Dec(LDestLen);
          end;
        else
          Exit(0); // Return a length of 0
      end;
    end;
  end;

  if LocaleStrLen = 0 then
    Result := LSize + Length(LCalcSizeBuf) - LDestLen
  else
    Result := LocaleStrLen - LDestLen
  finally
    iconv_close(LContext);
  end;
end;

function UnicodeFromLocaleChars(const LocaleName: AnsiString; Flags: Cardinal;
  LocaleStr: PAnsiChar; LocaleStrLen: Integer; UnicodeStr: PWideChar;
  UnicodeStrLen: Integer): Integer; overload;
var
  LContext: iconv_t;
  InvalidCharFound: Boolean;
  LCalcSizeBuf: array of Byte;
  LDestLen, LSourceLen, LSize, LZero: Integer;
  LSourcePtr, LDestPtr, LNil, LastInvalidChar: Pointer;
begin
  Result := 0;
  LContext := iconv_open('UTF-16LE', PAnsiChar(LocaleName)); // do not localize
  if LContext <> iconv_t(-1) then
  try
  // When LocaleStrLen is -1 treat LocaleStr as a null terminated string
  if LocaleStrLen = -1 then
    LocaleStrLen := Length(LocaleStr) + 1; // include terminating null

  // UnicodeStrLen = 0 is a request to calculate the required buffer size,
  // use a local destination buffer for the conversion.
  if UnicodeStrLen = 0 then
  begin
    SetLength(LCalcSizeBuf, 1024);
    LDestLen := Length(LCalcSizeBuf);
    LDestPtr := Pointer(@LCalcSizeBuf[0]);
  end
  else
  begin
    LDestLen := UnicodeStrLen * SizeOf(WideChar);
    LDestPtr := Pointer(UnicodeStr);
  end;

  LSourceLen := LocaleStrLen;
  LSourcePtr := Pointer(LocaleStr);

  LSize := 0;
  LastInvalidChar := nil;
  InvalidCharFound := False;

  while True do
  begin
      Result := iconv(LContext, @LSourcePtr, @LSourceLen, @LDestPtr, @LDestLen);
    if Result <> -1 then
      Break
    else
    begin
      case GetLastError of
        E2BIG: // Insufficient destination buffer
          if UnicodeStrLen = 0 then
          begin
            // Save converted buffer size and reset to beginning of local buffer
            Inc(LSize, Length(LCalcSizeBuf));
            LDestPtr := Pointer(@LCalcSizeBuf[0]);
            LDestLen := Length(LCalcSizeBuf);
          end
          else
          begin
            // Return a length of 0
            LDestLen := UnicodeStrLen * SizeOf(WideChar);
            Break;
          end;
        EILSEQ: // Invalid character sequence in source string
          if LocaleName = 'UTF-7' then // do not localize
          begin
            // Special case for emulating MultiByteToWideChar with UTF-7.
            // This does not produce an exact match due to differences in the
            // decoders, but it preserves similar behaviour.
            if (Flags and MB_ERR_INVALID_CHARS = MB_ERR_INVALID_CHARS) then
              Exit(0);
            Inc(NativeInt(LSourcePtr), SizeOf(AnsiChar));
            Dec(LSourceLen, SizeOf(AnsiChar));
            // Reset state of context
            LNil := nil;
            LZero := 0;
              iconv(LContext, @LNil, @LZero, @LNil, @LZero);
          end
          else
          begin
            // Increment pointers and insert #$FFFD into the destination
            // string if the source is a UTF-8 string. Only insert #$FFFD
            // once per invalid UTF-8 character.
            if LSourcePtr <> LastInvalidChar then
            begin
              if (UnicodeStrLen <> 0) and (LocaleName = 'UTF-8') then // do not localize
                PWideChar(LDestPtr)^ := #$FFFD; // Invalid UTF-8 char
              Inc(NativeInt(LDestPtr), SizeOf(WideChar));
              Dec(LDestLen, SizeOf(WideChar));
            end;
            Inc(NativeInt(LSourcePtr), SizeOf(AnsiChar));
            Dec(LSourceLen, SizeOf(AnsiChar));
            LastInvalidChar := LSourcePtr;
            InvalidCharFound := True;
          end;
        else
          Exit(0); // Return a length of 0
      end;
    end;
  end;

  if InvalidCharFound and (Flags and MB_ERR_INVALID_CHARS = MB_ERR_INVALID_CHARS) then
    Exit(0); // Return 0 if an invalid character was encountered

  if UnicodeStrLen = 0 then
    Result := (LSize + Length(LCalcSizeBuf) - LDestLen) div SizeOf(WideChar)
  else
    Result := UnicodeStrLen - (LDestLen div 2);
  finally
    iconv_close(LContext);
  end;
end;
{$ENDIF POSIX}

procedure SetMultiByteConversionCodePage(CodePage: Integer);
begin
  DefaultSystemCodePage := CodePage;
end;

function GetCPUCount: Integer;
{$IFDEF MSWINDOWS}
var
  SysInfo: TSystemInfo;
begin
  GetSystemInfo(SysInfo);
  Result := SysInfo.dwNumberOfProcessors;
end;
{$ENDIF}
{$IFDEF POSIX}
begin
  Result := sysconf(_SC_NPROCESSORS_ONLN);
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
procedure SetUtf8CompareLocale;
var
  OSVersion: Cardinal;
  MajorVersion: Cardinal;
  MinorVersion: Cardinal;
begin
  OSVersion := GetVersion;
  MajorVersion := OSVersion and $000000FF;
  MinorVersion := (OSVersion and $0000FF00) shr 8;

  if ((MajorVersion = 5) and (MinorVersion >= 1)) or
     (MajorVersion > 5) then
    UTF8CompareLocale := LOCALE_INVARIANT
  else
    UTF8CompareLocale := $0409;
end;
{$ENDIF MSWINDOWS}

{$IFDEF POSIX}
var
  InternalUTF8CompareLocale: Pointer = nil;

function UTF8CompareLocale: Pointer;
begin
  Result := InternalUTF8CompareLocale;
end;

function SetUTF8CompareLocale(const LocaleName: string): Boolean;
var
  LNewLocale, OldLocale: Pointer;
begin
  LNewLocale := newlocale(LC_ALL_MASK, PAnsiChar(AnsiString(LocaleName)), nil);
  Result := LNewLocale <> nil;
  if Result then
  begin
    OldLocale := InterlockedExchangePointer(InternalUTF8CompareLocale, LNewLocale);
    if OldLocale <> nil then
      freelocale(OldLocale);
  end;
end;
{$ENDIF POSIX}

{$IFDEF MSWINDOWS}

{$IF defined(CPU386)}
{$L delayhlp.obj}

procedure LocalAlloc; external 'kernel32.dll' name 'LocalAlloc';
procedure LocalFree; external 'kernel32.dll' name 'LocalFree';
procedure LoadLibraryA; external 'kernel32.dll' name 'LoadLibraryA';
procedure lstrcmpiA; external 'kernel32.dll' name 'lstrcmpiA';
function InterlockedExchangePtr(Dest: Pointer; Value: Pointer): Pointer; stdcall; external 'kernel32.dll' name 'InterlockedExchange';
procedure __delayLoadHelper; external;
procedure __FUnloadDelayLoadedDLL; external;
procedure ___pfnDliNotifyHook; external;
procedure ___pfnDliFailureHook; external;
procedure _InitDelayHelp; external;
procedure _ShutdownDelayHelp; external;

{$ELSE}

procedure __delayLoadHelper;
begin
end;
procedure __FUnloadDelayLoadedDLL;
begin
end;
procedure ___pfnDliNotifyHook;
begin
end;
procedure ___pfnDliFailureHook;
begin
end;
procedure _InitDelayHelp;
begin
end;
procedure _ShutdownDelayHelp;
begin
end;
{$IFEND}

procedure _memcpy(var Dest; const Src; Count: Integer); cdecl;
begin
  Move(Src, Dest, Count);
end;

procedure _memset(var Dest; Value: Byte; Count: Integer); cdecl;
begin
  FillChar(Dest, Count, Value);
end;

function SetDliNotifyHook(HookProc: TDelayedLoadHook): TDelayedLoadHook;
begin
  Result := InterlockedExchangePtr(pfnDliNotifyHook, @HookProc);
end;

function DliNotifyHook: TDelayedLoadHook;
begin
  Result := InterlockedExchangePtr(pfnDliNotifyHook, PPointer(pfnDliNotifyHook)^);
end;

function SetDliFailureHook(HookProc: TDelayedLoadHook): TDelayedLoadHook;
begin
  Result := InterlockedExchangePtr(pfnDliFailureHook, @HookProc);
end;

function DliFailureHook: TDelayedLoadHook;
begin
  Result := InterlockedExchangePtr(pfnDliFailureHook, PPointer(pfnDliFailureHook)^);
end;





procedure _DelayLoadHelper;
{$IF not defined(CPU386)}
type
  TDelayLoadHelperProc = procedure;
begin
  TDelayLoadHelperProc(delayLoadHelper)();
end;
{$ELSE}
asm
     JMP [delayLoadHelper]
end;
{$IFEND}





procedure UnloadDelayLoadedDLL(szDll: PAnsiChar);
{$IF not defined(CPU386)}
type
  TUnloadDelayLoadedDLLProc = procedure(szDll: PAnsiChar);
begin
  TUnloadDelayLoadedDLLProc(UnloadDelayLoadedDLLPtr)(szDll);
end;
{$ELSE}
asm
     JMP [UnloadDelayLoadedDLLPtr]
end;
{$IFEND}

{$ENDIF MSWINDOWS}

class operator TGUID.Equal(const Left, Right: TGUID): Boolean;
var
  a, b: PIntegerArray;
begin
  a := PIntegerArray(@Left);
  b := PIntegerArray(@Right);
  Result := (a^[0] = b^[0]) and (a^[1] = b^[1]) and (a^[2] = b^[2]) and (a^[3] = b^[3]);
end;

class operator TGUID.NotEqual(const Left, Right: TGUID): Boolean;
begin
  Result := not (Left = Right);
end;

class function TGUID.Empty: TGUID;
begin
  FillChar(Result, Sizeof(Result), 0)
end;




initialization
{$IFDEF MSWINDOWS}
  _InitDelayHelp;
  InitializeMemoryManager;
  InitializeLocaleData;
{$ENDIF}
{$IFDEF POSIX}
  setlocale(LC_ALL, '');
{$ENDIF POSIX}

  FileMode := 2;

{$IFDEF MSWINDOWS}
  RaiseExceptionProc := @RaiseException;
  RTLUnwindProc := @RTLUnwind;
{$ENDIF MSWINDOWS}

{$IFDEF MSWINDOWS}
  Test8086 := 2;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  FileAccessRights := S_IRUSR or S_IWUSR or S_IRGRP or S_IWGRP or S_IROTH or S_IWOTH;
  Test8086 := GetCPUType;
  IsConsole := True;
  FindResourceCache.ModuleHandle := LongWord(-1);
  ReserveZeroPage;
{$ENDIF LINUX}
{$IFDEF MACOS}
  FileAccessRights := S_IRUSR or S_IWUSR or S_IRGRP or S_IWGRP or S_IROTH or S_IWOTH;
  Test8086 := 2;
  IsConsole := True;
//  FindResourceCache.ModuleHandle := LongWord(-1);
{$ENDIF MACOSX}
  CPUCount := GetCPUCount;

  DispCallByIDProc := @_DispCallByIDError;

  _FpuInit();

  TTextRec(Input).Mode := fmClosed;
  TTextRec(Output).Mode := fmClosed;
  TTextRec(ErrOutput).Mode := fmClosed;

{$IFDEF MSWINDOWS}
  CmdLine := GetCommandLine;
  CmdShow := GetCmdShow;
{$ENDIF MSWINDOWS}
  DefaultSystemCodePage := GetACP;
  DefaultUnicodeCodePage := CP_UTF16; // UTF16 - Do not pass to MultiByteToWideChar or WideCharToMultiByte
  MainThreadID := GetCurrentThreadID;

{$IFDEF MSWINDOWS}
  SetUtf8CompareLocale;
{$ENDIF MSWINDOWS}
{$IFDEF POSIX}
  SetUTF8CompareLocale('en_US.UTF-8');  // do not localize
{$ENDIF POSIX}

finalization
  Close(Input);
  Close(Output);
  Close(ErrOutput);
{$IFDEF LINUX}
  ReleaseZeroPage;
{$ENDIF LINUX}
{$IFDEF POSIX}
  if InternalUTF8CompareLocale <> nil then
    freelocale(InternalUTF8CompareLocale);
{$ENDIF POSIX}
{$IFDEF MSWINDOWS}
  FinalizeLocaleDate;
  {Uninitialize the default memory manager, and free all memory allocated by
   this memory manager.}
  FinalizeMemoryManager;
  _ShutdownDelayHelp;
{$ENDIF MSWINDOWS}
end.


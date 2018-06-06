(* ::Package:: *)

(*Place in FileNameJoin[{$UserBaseDirectory, "Applications"}] 
or with mmDEER notebook.

From https://github.com/szhorvat/RLinkX/blob/master/RLinkX.*)

BeginPackage["RLinkX`", {"RLink`"}]

InstallRX::usage = "InstallRX[] will launch an external version of R."

Begin["`Private`"]

(* Set the location of your R installation here: 
For Mac users see the note on El Cap and later users here: 
http://szhorvat.net/pelican/setting-up-rlink-for-mathematica.html
Follow solution number 2 (library patching) to get things working.
*)
Switch[$OperatingSystem,
    "MacOSX",  rloc = "/Library/Frameworks/R.framework/Resources/",
    "Unix",    rloc = "/usr/lib/R", (* "/usr/lib64/R" for Fedora *)
    "Windows", rloc = "C:\\Program Files\\R\\R-3.1.2"
]


tenone = OrderedQ[{{10., 1}, {$VersionNumber, $ReleaseNumber}}];

addLibPath[var_] := Module[{lpath, rlibloc},
    rlibloc = FileNameJoin[{rloc, "lib"}];
    lpath = var /. GetEnvironment[var]; 
    SetEnvironment[var -> If[lpath === None, rlibloc, lpath <> ":" <> rlibloc]]
]

Switch[$OperatingSystem,
    "MacOSX", 
    addLibPath["DYLD_LIBRARY_PATH"];
    SetOptions[RLink`InstallR, "RHomeLocation" -> rloc];
    If[tenone, SetOptions[RLink`InstallR, "RVersion" -> 3]];
    ,
    "Unix",
    (* addLibPath["LD_LIBRARY_PATH"]; *)
    SetOptions[RLink`InstallR, "RHomeLocation" -> rloc];
    If[tenone, SetOptions[RLink`InstallR, "RVersion" -> 3]];
    ,
    "Windows",
    SetOptions[RLink`InstallR, "RHomeLocation" -> rloc];
]

InstallRX[] := 
  RLink`InstallR["RHomeLocation" -> rloc, 
    If[$OperatingSystem =!= "Windows" && tenone,
      "RVersion" -> 3,
      Unevaluated@Sequence[]
    ]
  ]

End[] (* `Private` *)

EndPackage[]

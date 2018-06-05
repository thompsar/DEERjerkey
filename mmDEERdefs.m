(* ::Package:: *)

(* ::Input::Initialization:: *)
(*function definitions*)
(*Fit to N gaussian populations*)
ListMean[x_]:=Total[x]/Total@Unitize@x;

peakfunc[\[Mu]_,A_,\[Sigma]_,x_]:=A*E^(-((x-\[Mu])^2/(2 \[Sigma]^2)));

gaussianmodelzero[data_,populations_]:=Module[{dataconfig,modelfunc,fitvar,initialconditions},dataconfig={\[Mu][#],A[#],\[Sigma][#]}&/@Range[Length[populations]];
modelfunc=peakfunc[##,fitvar]&@@@dataconfig//Total;
initialconditions=ArrayReshape[Riffle[Flatten[populations],.5,{3,-1,3}],{Length[populations],3}];
FindFit[data,modelfunc,{dataconfig//Flatten,initialconditions//Flatten}\[Transpose],fitvar]];

gaussianmodelzero2[data_]:=Module[{dataconfig,modelfunc,fitvar,initialconditions},dataconfig={\[Mu][#],A[#],\[Sigma][#]}&/@Range[1];
modelfunc=peakfunc[##,fitvar]&@@@dataconfig//Total;
initialconditions=ArrayReshape[Riffle[{1,0},.5,{3,-1,3}],{1,3}];
dataconfig/.FindFit[data,modelfunc,{dataconfig//Flatten,initialconditions//Flatten}\[Transpose],fitvar]];

gaussianmodel[data_,populations_]:=Module[{dataconfig,modelfunc,fitvar,initialconditions},dataconfig={\[Mu][#],A[#],\[Sigma][#]}&/@Range[Length[populations]];
modelfunc=peakfunc[##,fitvar]&@@@dataconfig//Total;
initialconditions=ArrayReshape[Riffle[Flatten[populations],.5,{3,-1,3}],{Length[populations],3}];
FindFit[data,{modelfunc,{11>\[Mu][#]>1.5,A[#]>0,1>\[Sigma][#]>.01}&/@Range[Length[populations]]//Flatten},{dataconfig//Flatten,initialconditions//Flatten}\[Transpose],fitvar]];

gaussianresult[fitresult_,r_]:=Module[{dataconfig,modelfunc},dataconfig={\[Mu][#],A[#],\[Sigma][#]}&/@Range[Length[fitresult]/3];
If[Length[fitresult]!=0,
Total@(peakfunc[##,r]&@@@dataconfig/.fitresult)]

];

cbcolor[x_]:=Module[{cbpalette,cbscheme},
cbpalette=RGBColor@@@({{51,34,136},{102,153,204},{136,204,238},{68,170,153},{17,119,51},{153,153,51},{221,204,119},{102,17,0},{204,102,119},{170,68,102},{136,34,85},{170,68,153},{68,119,170}}/255);
cbscheme={{13},{13,9},{13,7,9},{13,5,7,9},{1,3,5,7,9},{1,3,5,7,9,12},{1,3,4,5,7,9,12},{1,3,4,5,6,7,9,12},{1,3,4,5,6,7,9,11,12},{1,3,4,5,6,7,8,9,11,12},{1,2,3,4,5,6,7,8,9,11,12},{1,2,3,4,5,6,7,8,9,10,11,12}};
If[x<=12,
cbpalette[[#]]&[cbscheme[[x]]],
cbpalette[[#]]&[PadRight[cbscheme[[12]],x,cbscheme[[12]]]]
]
];

color[x_]:=ColorData[97][x];


labelT[plot_,opts___]:=Labeled[plot,{" ","Evolution Time (\[Mu]s)"},{Left,Bottom},RotateLabel->True,opts,LabelStyle->helv[24],ImageSize->500,Spacings->0];
specPlot[specs_,opts___]:=ListPlot[specs,opts,Joined->True,PlotRange->All,AxesOrigin->{0,-.2},LabelStyle->helv[24],ImageSize->500];

labelR[plot_,opts___]:=Labeled[plot,{" ","Distance (nm)"},{Left,Bottom},RotateLabel->True,opts,LabelStyle->helv[24],ImageSize->500,Spacings->0];
distrPlot[distr_,opts___]:=ListPlot[distr,opts,Joined->True,PlotRange->All,LabelStyle->helv[24],ImageSize->500];

(*SpecPlot[specs_,opts:OptionsPattern[]]:=Module[{},Labeled[ListPlot[specs,opts,Joined\[Rule]True,PlotRange\[Rule]All,AxesOrigin\[Rule]{0,-.2},LabelStyle\[Rule]helv[24],ImageSize\[Rule]500],{" ","Evolution Time (\[Mu]s)"},{Left,Bottom},RotateLabel\[Rule]True,LabelStyle\[Rule]helv[24],ImageSize\[Rule]500,Spacings\[Rule]0]];

DistrPlot[distr_,opts:OptionsPattern[]]:=Module[{},Labeled[ListPlot[distr,opts,Joined\[Rule]True,PlotRange\[Rule]All,AxesOrigin\[Rule]{0,0},LabelStyle\[Rule]helv[24],ImageSize\[Rule]500],{" ","Distance (nm)"},{Left,Bottom},RotateLabel\[Rule]True,LabelStyle\[Rule]helv[24],ImageSize\[Rule]500,Spacings\[Rule]0]];
*)
tabletext[fitresult_,color_]:=Module[{totalarea,partialareas},
totalarea=NIntegrate[Total@(peakfunc[##,x]&@@@({\[Mu][#],A[#],\[Sigma][#]}&/@Range[(fitresult//Length)/3])/.fitresult),{x,kernelstart,kernelend}];

partialareas=Table[NIntegrate[Total@(peakfunc[##,x]&@@@({\[Mu][#],A[#],\[Sigma][#]}&/@{i})/.fitresult),{x,kernelstart,kernelend}],{i,(fitresult//Length)/3}];

partialareas=partialareas/totalarea;

Style[NumberForm[TableForm[Table[{\[Mu][i],Abs[\[Sigma][i]],partialareas[[i]]},{i,1,Length[fitresult]/3}]/.fitresult,TableAlignments->Center,TableHeadings->{CharacterRange["1",ToString[Length[fitresult]/3]],{"r (nm)","\[Sigma] (nm)","frac"}}],{3,2}],helv[18],color]];

tabletext2[fitresult_,color_]:=Module[{totalarea,partialareas},
totalarea=NIntegrate[Total@(peakfunc[##,x]&@@@fitresult),{x,kernelstart,kernelend}];

partialareas=Table[NIntegrate[Total@(peakfunc[##,x]&@@@{fitresult[[i]]}),{x,kernelstart,kernelend}],{i,Length@fitresult}];

partialareas=partialareas/totalarea;

Style[NumberForm[TableForm[Table[{fitresult[[i,1]],Abs[fitresult[[i,3]]],partialareas[[i]]},{i,1,Length@fitresult}],TableAlignments->Center,TableHeadings->{CharacterRange["1",ToString[Length@fitresult]],{"r (nm)","\[Sigma] (nm)","frac"}}],{3,2}],helv[18],color]];

tabletextMC[fitresult_,color_]:=Module[{},


Style[NumberForm[TableForm[Table[{\[Mu][i],\[Sigma][i],A[i]},{i,1,Length[fitresult]/3}]/.fitresult,TableAlignments->Center,TableHeadings->{CharacterRange["1",ToString[Length[fitresult]/3]],{"r (nm)","\[Sigma] (nm)","frac"}}],{3,2}],helv[18],color]];

tablelesstext[fitresult_]:=Module[{totalarea,partialareas},
totalarea=NIntegrate[Total@(peakfunc[##,x]&@@@({\[Mu][#],A[#],\[Sigma][#]}&/@Range[(fitresult//Length)/3])/.fitresult),{x,kernelstart,kernelend}];

partialareas=Table[NIntegrate[Total@(peakfunc[##,x]&@@@({\[Mu][#],A[#],\[Sigma][#]}&/@{i})/.fitresult),{x,kernelstart,kernelend}],{i,(fitresult//Length)/3}];

partialareas=partialareas/totalarea;
Table[{\[Mu][i],Abs[\[Sigma][i]],partialareas[[i]]},{i,1,Length[fitresult]/3}]/.fitresult];

bitsToIndices[v_]:=Select[Table[i,{i,Length[v]}],v[[#]]==1&];
getrange[list_,start_,end_]:=Position[list,Nearest[list,start][[1]]][[1,1]];;Position[list,Nearest[list,end][[1]]][[1,1]];

tikhonovfunc[\[Alpha]_,kernel_,Lmatrix_,bvector_]:=PseudoInverse[Transpose[kernel].kernel+\[Alpha]^2*Lmatrix\[Transpose].Lmatrix].Transpose[kernel].bvector;
hMat[\[Alpha]_,kernel_,Lmatrix_]:=kernel.PseudoInverse[Transpose[kernel].kernel+\[Alpha]^2*Lmatrix\[Transpose].Lmatrix].Transpose[kernel];

backgroundCorrect[spectrumY_,backgroundY_,Y0_]:=Module[{Y0new,waveformY},
Y0new=(Y0-backgroundY[[1]])/(backgroundY[[1]]);
waveformY=(spectrumY-backgroundY)/backgroundY;
waveformY=waveformY/Y0new];

backgroundAdd[waveform00_,background_,extrabg_,zeroamp_]:=Module[{rescale,waveform0,waveform},
rescale=(zeroamp-background[[1]])/(background[[1]]);
waveform0=waveform00*(1.0-extrabg[[1]])+extrabg;
waveform=waveform0*rescale*background+background];

helv[size_]:=Directive[FontFamily->"Helvetica",FontSize->size,Black];

gausswave[parameters_,kernel_,r_]:=Module[{waveform},
waveform=kernel.Total@(peakfunc[##,r]&@@@parameters);
waveform=waveform/waveform[[1]]
]

gradientD[fitwaveform_,startposition_,kernel_,r_]:=Module[{stepsize=.001,mycounter=1,dRSS=1,lastRSS=10,myRSS,partialDs,myposition=startposition,numgaussians=Dimensions[startposition][[1]],sparsearrays},

sparsearrays=Table[SparseArray[{i,j}->.0001,{numgaussians,3}],{i,numgaussians},{j,3}];

While[mycounter<5000&&dRSS>1*10^-6,
{
myRSS=Norm[fitwaveform-gausswave@@{myposition,kernel,r}]^2;

dRSS=Abs[lastRSS-myRSS];
lastRSS=myRSS;

partialDs=Table[
(Norm[fitwaveform-gausswave@@{(myposition+sparsearrays[[i,j]]),kernel,r}]^2-Norm[fitwaveform-gausswave@@{myposition,kernel,r}]^2)/.0001
,{i,numgaussians},{j,3}];

myposition=Abs[myposition-stepsize*partialDs];


mycounter++;

}];
myposition];

aicc[data_,fit_,Q_]:=Module[{N=Length[fit],K=Q+1},

N*Log[(Norm[data-fit]^2)/N]+2*K+(2K(K+1))/(N-K-1)
];

bic[data_,fit_,Q_]:=Module[{N=Length[fit],K=Q+1},

N*Log[(Norm[data-fit]^2)/N]+K*Log[N]
];

(*kernel construction*)
buildkernels[dimension_,rmin_,rmax_,dt_]:=Module[{ny0,w0,t,r,kernel,k,i,Lmatrix},
ny0=52.04;(* dipolar frequency at 1 nm for g=ge*)
w0=2*\[Pi]*ny0;(* angular frequencies*)
t=Table[i,{i,0,(dimension-1)*dt,dt}]; (*time axis in \[Micro]s*)
r=Table[i,{i,rmin,rmax,(rmax-rmin)/(dimension-1)}];(* distance axis in nm*)

kernel=ConstantArray[0.,{Length[r],Length[t]}];(*  data array for kernel*)

 kernel=ParallelTable[Sum[Cos[(w0/r[[k]]^3*(3*(l/1000)^2-1))*t],{l,0,1000}],{k,1,Length[r]}];

For[ k=1,k<=Length[r] ,k++,
(*loop for kernel normalization*)
kernel[[k,All]]=kernel[[k,All]]/kernel[[k,1]]; (*normalize dipolar time evolution traces*)
];

kernel=kernel\[Transpose]; (*Rotate kernel for all other operations*)

Lmatrix=ConstantArray[0.,{dimension-2,dimension}];
For[i=1,i<=(Dimensions[Lmatrix][[1]]),i++,
Lmatrix[[i,1+(i-1)]]=1.;
Lmatrix[[i,1+(i-0)]]=-2.;
Lmatrix[[i,1+(i+1)]]=1.;
];
Lmatrix=SparseArray[Lmatrix];



{r,t,kernel,Lmatrix}
];

buildkernels2[dimension_,rmin_,rmax_,tmax_,dt_]:=Module[{ny0,w0,t,r,kernel,k,i,Lmatrix},
ny0=52.04;(* dipolar frequency at 1 nm for g=ge*)
w0=2*\[Pi]*ny0;(* angular frequencies*)
t=Table[i,{i,0,tmax+2*dt,dt}]; (*time axis in \[Micro]s*)
r=Table[i,{i,rmin,rmax,(rmax-rmin)/(dimension-1)}];(* distance axis in nm*)

kernel=ConstantArray[0.,{Length[r],Length[t]}];(*  data array for kernel*)

 kernel=ParallelTable[Sum[Cos[(w0/r[[k]]^3*(3*(l/1000)^2-1))*t],{l,0,1000}],{k,1,Length[r]}];

For[ k=1,k<=Length[r] ,k++,
(*loop for kernel normalization*)
kernel[[k,All]]=kernel[[k,All]]/kernel[[k,1]]; (*normalize dipolar time evolution traces*)
];

kernel=kernel\[Transpose]; (*Rotate kernel for all other operations*)

Lmatrix=ConstantArray[0.,{dimension-2,dimension}];
For[i=1,i<=(Dimensions[Lmatrix][[1]]),i++,
Lmatrix[[i,1+(i-1)]]=1.;
Lmatrix[[i,1+(i-0)]]=-2.;
Lmatrix[[i,1+(i+1)]]=1.;
];
Lmatrix=SparseArray[Lmatrix];



{r,t,kernel,Lmatrix}
];


correctedkernel[dimension_,rmin_,rmax_,tmax_,dt_,excite_]:=Module[{ny0,w0,t,r,kernel,k,i,Lmatrix},
ny0=52.04;(* dipolar frequency at 1 nm for g=ge*)
w0=2*\[Pi]*ny0;(* angular frequencies*)
t=Table[i,{i,0,tmax+2*dt,dt}]; (*time axis in \[Micro]s*)
r=Table[i,{i,rmin,rmax,(rmax-rmin)/(dimension-1)}];(* distance axis in nm*)

kernel=ConstantArray[0.,{Length[r],Length[t]}];(*  data array for kernel*)

kernel=ParallelTable[Sum[Exp[-(w0/r[[k]]^3)/(excite)^2]Cos[(w0/r[[k]]^3*(3*(l/1000)^2-1))*t],{l,0,1000}],{k,1,Length[r]}];

For[ k=1,k<=Length[r] ,k++,
(*loop for kernel normalization*)
kernel[[k,All]]=kernel[[k,All]]/kernel[[k,1]]; (*normalize dipolar time evolution traces*)
];

kernel=kernel\[Transpose]; (*Rotate kernel for all other operations*)




kernel
];

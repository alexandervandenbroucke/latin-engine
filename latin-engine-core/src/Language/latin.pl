:- discontiguous declension/4.

%% Declension I
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declension(ros-a,    nom, f, s).
declension(ros-am,   acc, f, s).
declension(ros-ae,   gen, f, s).
declension(ros-ae,   dat, f, s).
declension(ros-a,    abl, f, s).
declension(ros-a,    voc, f, s).
declension(ros-ae,   nom, f, p).
declension(ros-as,   acc, f, p).
declension(ros-arum, gen, f, p).
declension(ros-is,   dat, f, p).
declension(ros-is,   abl, f, p).
declension(ros-ae,   voc, f, p).


%% Declension II
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declension(av-us,  nom, m, s).
declension(av-um,  acc, m, s).
declension(av-i,   gen, m, s).
declension(av-o,   dat, m, s).
declension(av-o,   abl, m, s).
declension(av-e,   voc, m, s).
declension(av-i,   nom, m, p).
declension(av-os,  acc, m, p).
declension(av-orum,gen, m, p).
declension(av-is,  dat, m, p).
declension(av-is,  abl, m, p).
declension(av-i,   voc, m, p).

declension(don-um,   nom, n, s).
declension(don-um,   acc, n, s).
declension(don-i,    gen, n, s).
declension(don-o,    dat, n, s).
declension(don-o,    abl, n, s).
declension(don-um,   voc, n, s).
declension(don-a,    nom, n, p).
declension(don-a,    acc, n, p).
declension(don-orum, gen, n, p).
declension(don-is,   dat, n, p).
declension(don-is,   abl, n, p).
declension(don-a,    voc, n, p).

%% Declension III
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declension(dux,      nom, m, s).
declension(duc-em,   acc, m, s).
declension(duc-is,   gen, m, s).
declension(duc-i,    dat, m, s).
declension(duc-e,    abl, m, s).
declension(dux,      voc, m, s).
declension(duc-es,   nom, m, p).
declension(duc-es,   acc, m, p).
declension(duc-um,   gen, m, p).
declension(duc-ium,  gen, m, p).
declension(duc-ibus, dat, m, p).
declension(duc-ibus, abl, m, p).
declension(duc-es,   voc, m, p).

declension(mater,Case,  f, Number) :- declension(dux,Case,_Gender,Number).
declension(matr-I,Case, f, Number) :- declension(duc-I,Case,_Gender,Number).

declension(corpus,      nom, n, s).
declension(corpus,      acc, n, s).
declension(corpor-is,   gen, n, s).
declension(corpor-i,    dat, n, s).
declension(corpor-e,    abl, n, s).
declension(corpus,      voc, n, s).
declension(corpor-a,    nom, n, p).
declension(corpor-a,    acc, n, p).
declension(corpor-um,   gen, n, p).
declension(corpor-ibus, dat, n, p).
declension(corpor-ibus, abl, n, p).
declension(corpor-a,    voc, n, p).

%% Declension IV
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declension(fruct-us,   nom, m, s).
declension(fruct-um,   acc, m, s).
declension(fruct-us,   gen, m, s).
declension(fruct-ui,   dat, m, s).
declension(fruct-u,    abl, m, s).
declension(fruct-us,   voc, m, s).
declension(fruct-us,   nom, m, p).
declension(fruct-us,   acc, m, p).
declension(fruct-uum,  gen, m, p).
declension(fruct-ibus, dat, m, p).
declension(fruct-ibus, abl, m, p).
declension(fruct-us,   voc, m, p).

declension(man-I, Case, f, Number) :- declension(fruct-I, Case, m, Number).

declension(corn-u,    nom, n, s).
declension(corn-u,    acc, n, s).
declension(corn-us,   gen, n, s).
declension(corn-ui,   dat, n, s).
declension(corn-u,    abl, n, s).
declension(corn-u,    voc, n, s).
declension(corn-ua,   nom, n, p).
declension(corn-ua,   acc, n, p).
declension(corn-uum,  gen, n, p).
declension(corn-ibus, dat, n, p).
declension(corn-ibus, abl, n, p).
declension(corn-ua,   voc, n, p).

%% Declension V
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

declension(di-es,   nom, m, s).
declension(di-em,   acc, m, s).
declension(di-ei,   gen, m, s).
declension(di-ei,   dat, m, s).
declension(di-e,    abl, m, s).
declension(di-es,   voc, m, s).
declension(di-es,   nom, m, p).
declension(di-es,   acc, m, p).
declension(di-erum, gen, m, p).
declension(di-ebus, dat, m, p).
declension(di-ebus, abl, m, p).
declension(di-es,   voc, m, p).

declension(di-I, Case, f, Number) :- declension(di-I, Case, m, Number).

%% Signatures
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

signature(arbor, arbor-is, f, iii).
signature(astrum, astr-i, n, ii).
signature(avus,av-i,m,ii).
signature(canis, can-is,m,iii).
signature(cornu, corn-us,n,iv).
signature(corpus,corpor-is,n,iii).
signature(dies, di-ei,m,v).
signature(dies, di-ei,f,v).
signature(donum,don-i,n,ii).
signature(dux,   duc-is,m,iii).
signature(frater,fratr-is,m,iii).
signature(fructus, fruct-us,m,iv).
signature(luna, lun-ae,f,i).
signature(lupus, lup-i, m, ii).
signature(manus, man-us,f,iv).
signature(mater, matr-is,f,iii).
signature(res, r-ei, f, v).
signature(rosa,ros-ae,f,i).
signature(soror, soror-is,f,iii).
signature(stella, stell-ae,f,i).
signature(templum,templ-i,n,ii).
signature(tempus,temp-oris,n,iii).

%% Automatic Declining
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

stem(Stem) :- signature(Stem,_,_,_), Stem \= _-_.
stem(Stem) :- signature(_,Stem-_,_,_).

decline(Form,Case,Gender,Number,Nom) :-
    signature(Nom,Stem-_,Gender,Declension),
    ( % Nominative case for III-rd declension
      Case = nom,
      Number = s,
      Form = Nom

    ; % Vocative case for III-rd declension
      Case = voc, Number = s, Form = Nom,
      declension(Voc, Case, Gender, s),
      signature(Voc,_,Gender,Declension)
    
    ; % General case
      declension(Stem2-I, Case, Gender, Number),
      signature(_,Stem2-_,Gender,Declension),
      Form = Stem-I
    
    ).

decline_atom(Form,Case,Gender,Number,Nom) :-
    Nom = Form,
    decline(Form,Case,Gender,Number,Nom).
decline_atom(Form,Case,Gender,Number,Nom) :-
    decline(Stem-I,Case,Gender,Number,Nom),
    atom_concat(Stem,I,Form).

%% ?- decline_atom(ducibus,Case,Gender,Number,Nom).
%% Case = dat,
%% Gender = m,
%% Number = p,
%% Nom = dux ;
%% Case = abl,
%% Gender = m,
%% Number = p,
%% Nom = dux ;
%% false.

% It may be more efficient to start searching from the rear.

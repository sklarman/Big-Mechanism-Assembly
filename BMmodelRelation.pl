%% Program pre-processing an OWL/RDF file for the use in ProbLog
%%
%% The four type of probabilities over BEL statements are assigned randomly
%%
%%

%shell('d:/problog.bat').

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/sparql_client)).
:- use_module(library(http/json)).
:- use_module(library(http/http_open)).

:- use_module(library(gensym)).
:- reset_gensym.


:- rdf_load('PAF.owl', [graph(pafOnto)]).
:- rdf_load('graph.rdf', [graph(graph)]).
%:- rdf_load('mapping.rdf', [graph(mapping)]).
:- rdf_load('Ras-2-neighborhood.owl', [graph(ras)]).
:- rdf_load('biopax-level3.owl', [graph(biopax)]).

:- rdf_register_prefix(mod, 'http://purl.org/pc2/7/#').
:- rdf_register_prefix(bp, 'http://www.biopax.org/release/biopax-level3.owl#').
%:- rdf_register_prefix(unprot, 'http://identifiers.org/uniprot/').
%:- rdf_register_prefix(cheb, 'http://identifiers.org/chebi/CHEBI:').
:- rdf_register_prefix(bm, 'http://www.semanticweb.org/big-mechanism-data#').
:- rdf_register_prefix(bmpaf, 'http://www.semanticweb.org/ontologies/2015/7/untitled-ontology-160#').
:- rdf_register_prefix(bmp, 'http://www.semanticweb.org/ontologies/2015/6/untitled-ontology-155#').
:- rdf_register_prefix(owl, 'http://www.w3.org/2002/07/owl#').
:- rdf_register_prefix(xsd, 'http://www.w3.org/2001/XMLSchema#').
:- rdf_register_prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_register_prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- prolog_load_context(directory, Dir),  asserta(user:file_search_path(my_home, Dir)).


%% Defining an auxiliary predicate

:- dynamic aux/3.
:- dynamic bindMatch/6.
:- rdf_meta aux(r, r, r).


%% RDFS materialization:

rdf_add(X, Y, Z):- \+rdf(X, Y, Z), \+rdf_is_literal(X), rdf_assert(X, Y, Z).


aux(X, Y, Z) :- rdf(X, Y, Z). 
aux(X, Y, Z) :- Y \== 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', rdf(V, rdfs:subPropertyOf, Y), aux(X, V, Z), rdf_add(X, Y, Z).
aux(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Z) :- X \== Z, rdf(Y, rdfs:range, Z), aux(_, Y, X), rdf_add(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Z). 
aux(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Z) :- X \== Z, rdf(Y, rdfs:domain, Z), aux(X, Y, _), rdf_add(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Z). 
aux(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Z) :-  X \== Z, rdf(Y, rdfs:subClassOf, Z), aux(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Y), rdf_add(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Z). 


%%%%%%%%%%%%

targetGraph(mapping).

run_instance_match :-
    targetGraph(Graph), !,
    writeln('Matching participants using hasExactMatch and hasPotentialMatch properties, where:'),
    writeln('X hasExactMatch Y, whenever X and Y share the same UNIPORT or CHEBI identifier;'),
    writeln('X hasPotentialMatch Y, whenever X and Y share some textual labels only.'),
    countBmmol(Number1),
    countBpmol(Number2),
    Number3 is Number1*Number2,
    writeln(Number3),
    findall(_, moleculeMatch(_, _), _),
    writeln('Done exact match.'),
    addMetaData,
    saveGraph(Graph),
    reset_gensym,
    findall(_, moleculeMatchNoId(_, _), _),
    writeln('Done potential match.'),
    saveGraph(Graph).


moleculeMatch(BMol, BPmol) :-
    print_count100,
    bmMolecule(BMol, Identifier),
    bpMolecule(BPmol, Identifier),
    checkForChebi(BPmol, BPmolFixed),
    writeInTargetGraph(BMol, bm:'hasExactMatch', BPmolFixed).

%for the chebi case you need to fix the issue that ':' is replaced with '%3A' in Prolog....

print_count100 :-
    gensym('', Counter), 
    atom_number(Counter, Number), 
    0 is mod(Number, 10000) -> writeln(Number); true.

bmmol(BMol):-
    rdf(BMol, rdf:'type', BmType), 
    rdfs_subclass_of(BmType, bmpaf:'Chemical'),
    print_count100.

bpmol(BPmol):-
    rdf(BPmol, rdf:'type', BpType),
    rdfs_subclass_of(BpType, bp:'EntityReference'),
    print_count100.
    
countBmmol(Number) :-
    findall(BMol, bmmol(BMol), L),
    length(L, Number).
    
countBpmol(Number) :-
    findall(BPmol, bpmol(BPmol), L),
    length(L, Number).   

bmMolecule(BMol, BMolIdUpper) :- 
    bmmol(BMol),
    rdf(BMol, bmpaf:'hasEntityId', LabelLiteral),
    rdf_literal_value(LabelLiteral, BMolId),
    string_upper(BMolId, BMolIdUpper).

bpMolecule(BPmol, BPid) :- 
    bpmol(BPmol),
    identifierMatch(BPmol, BPid).    
    
checkForChebi(BPmol, BPmolFixed) :-
    string_concat('http://identifiers.org/chebi/CHEBI%3A', Number, BPmol),
    string_concat('http://identifiers.org/chebi/CHEBI:', Number, BPmolFixedString),
    atom_string(BPmolFixed, BPmolFixedString).

checkForChebi(BPmol, BPmol) :-
    \+string_concat('http://identifiers.org/chebi/CHEBI%3A', _, BPmol).

identifierMatch(BPid, BMid) :-
    string_concat('http://identifiers.org/chebi/CHEBI%3A', Number, BPid),
    string_concat('CHEBI:', Number, BMid).
    
identifierMatch(BPid, BMid) :-
    string_concat('http://identifiers.org/uniprot/', Number, BPid),
    string_concat('UNIPROT:', Number, BMid).

moleculeMatchNoId(BMol, BPmol) :-
    print_count100,
    bmMoleculeNoId(BMol, Name),
    bpMoleculeNoId(BPmol, Name),
    checkForChebi(BPmol, BPmolFixed),
    \+rdf(BMol, bm:'hasExactMatch', BPmolFixed),
    writeInTargetGraph(BMol, bm:'hasPotentialMatch', BPmolFixed).

bmMoleculeNoId(BMol, BMolIdUpper) :- 
    bmmol(BMol),
%   \+readInTargetGraph(BMol, bm:'hasExactMatch', _),
    hasTextualLabel(BMol, LabelLiteral),
    rdf_literal_value(LabelLiteral, BMolId),
    string_upper(BMolId, BMolIdUpper).

bpMoleculeNoId(BPmol, BPNameUpper) :- 
    bpmol(BPmol),
 %   \+rdf(BPmol, bp:'memberEntityReference', _),
    bpReferenceName(BPmol, NameLiteral),
    rdf_literal_value(NameLiteral, BPName),
    string_upper(BPName, BPNameUpper).    
    
    
bpReferenceName(BPmol, Name) :-
      rdf(BPmol, bp:'name', Name).

bpReferenceName(BPmol, Name) :-
      rdf(BPmol, bp:'displayName', Name).
      
bpReferenceName(BPmol, Name) :-
      rdf(BPmol, bp:'standardName', Name).

hasTextualLabel(BMol, LabelLiteral) :- 
     rdf(BMol, bmpaf:'hasEntityText', LabelLiteral).

hasTextualLabel(BMol, LabelLiteral) :- 
     rdf(BMol, bmpaf:'hasEntityName', LabelLiteral).
    


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

writeInTargetGraph(X, Y, Z) :-
    targetGraph(Graph),
    rdf_global_id(X, Xg),
    rdf_global_id(Y, Yg),
    rdf_global_id(Z, Zg),
    rdf_assert(Xg, Yg, Zg, Graph).

readInTargetGraph(X, Y, Z) :-
    targetGraph(Graph),
    rdf_global_id(X, Xg),
    rdf_global_id(Y, Yg),
    rdf_global_id(Z, Zg),
    rdf(Xg, Yg, Zg, Graph).


%%%%%%%%%%%

addMetaData :-
    writeInTargetGraph(bm:'hasPotentialMatch', rdf:'type', owl:'ObjectProperty'),
    writeInTargetGraph(bm:'hasExactMatch', rdf:'type', owl:'ObjectProperty'),
    writeInTargetGraph(bm:'hasMatch', rdf:'type', owl:'ObjectProperty'),
    writeInTargetGraph(bm:mapping, rdf:'type', owl:'Ontology'),
    writeInTargetGraph(bm:mapping, owl:'imports', 'http://www.semanticweb.org/big-mechanism-data#graph'),
    writeInTargetGraph(bm:mapping, owl:'imports', 'http://purl.org/pc2/7/').



%%%%%%%

saveGraph(Graph) :- 
    file_search_path(my_home, Dir),
    atomic_list_concat([Dir, '/', Graph, '.rdf'], OutputPath),
    rdf_save(OutputPath, [graph(Graph), encoding(utf8)]), !.








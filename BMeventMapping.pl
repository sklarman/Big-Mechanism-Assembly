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
:- rdf_load('mapping.rdf', [graph(mapping)]).
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
:- dynamic match/6.
:- rdf_meta aux(r, r, r).


%% RDFS materialization:

rdf_add(X, Y, Z):- \+rdf(X, Y, Z), \+rdf_is_literal(X), rdf_assert(X, Y, Z).


aux(X, Y, Z) :- rdf(X, Y, Z). 
aux(X, Y, Z) :- Y \== 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', rdf(V, rdfs:subPropertyOf, Y), aux(X, V, Z), rdf_add(X, Y, Z).
aux(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Z) :- X \== Z, rdf(Y, rdfs:range, Z), aux(_, Y, X), rdf_add(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Z). 
aux(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Z) :- X \== Z, rdf(Y, rdfs:domain, Z), aux(X, Y, _), rdf_add(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Z). 
aux(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Z) :-  X \== Z, rdf(Y, rdfs:subClassOf, Z), aux(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Y), rdf_add(X, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', Z). 


%%%%%%%%%%%%

targetGraph(eventMap).

run_event_match :-
    targetGraph(Graph), !,
    writeln('Searching for matching bindings...'),
    findall(_, matchingEvent(_, _, _, _, 'binding', _), _),
    writeln('Searching for matching regulation events...'),
    findall(_, matchingEvent(_, _, _, _, 'regulation', _), _),
    writeln('Selecting and saving top matches...'),
    findall(_, encodeMinimal(_), _),
    saveGraph(Graph).

%eventType('binding').
%eventType('regulation').
  
matchingEvent(Ev, E1, E2, Graph, 'binding', Uncertainty) :-
    rdf(Ev, rdf:'type', bmpaf:'Binding'),
    hasBmParticipants(Ev, Part1, Part2),
    hasMatch(Part1, E1, P1),
    hasMatch(Part2, E2, P2),
    bpBindingPattern(E1, E2, Uncert, Graph),
    Uncertainty is P1*P2*Uncert,
    gensym('', Counter),
    writeln(Counter),
    assert(match(Ev, E1, E2, Graph, 'binding', Uncertainty)).
    
matchingEvent(Ev, BmPart1, BmPart2, Graph, 'regulation', Uncertainty)  :-
    bpControl(ControllerRef, ControlledRef, ControlType, Uncert, Graph),
    hasMatch(BmPart1, ControllerRef, Unc1), 
    hasMatch(BmPart2, ControlledRef, Unc2),
    regulation(ControlType, Types),
    hasBmParticipants(Ev, BmPart1, BmPart2),
    rdf(Ev, rdf:'type', RegType),
    member(RegType, Types),
    Uncertainty is Unc1 * Unc2 * Uncert,
    gensym('', Counter),
    writeln(Counter),
    assert(match(Ev, BmPart1, BmPart2, Graph, 'regulation', Uncertainty)).

bpBinding(E1, E2, Uncertainty, [[B1, bp:'bindsTo', B2]|Graph]) :-
    bpBindingElement(E1, B1, P1, Graph1),
    rdf(B1, bp:'bindsTo', B2),
    bpBindingElement(E2, B2, P2, Graph2),
    append(Graph1, Graph2, Graph),
    Uncertainty is P1*P2.
    
bpBindingElement(E, B, 1, Graph) :-
    rdf(E, bp:'entityFeature', B),
    Graph = [[E, bp:'entityFeature', B]].

bpBindingElement(E, B, 0.5, Graph) :-
    rdf(Complex, bp:'component', Component),
    rdf(Component, bp:'entityReference', E),
    rdf(Complex, bp:'feature', B),
    Graph = [[Complex, bp:'component', Component], [Component, bp:'entityReference', E], [Complex, bp:'feature', B]].
    
bpBindingElement(E, B, 0.9, Graph) :-
    rdf(Element, bp:'entityReference', E),
    rdf(Element, bp:'feature', B),
    Graph = [[Element, bp:'entityReference', E],[Element, bp:'feature', B]].

bpBindingElement(E, B, 0.5, Graph) :-
    rdf(Element, bp:'memberEntityReference', E),
    rdf(Element, bp:'entityFeature', B),
    Graph = [[Element, bp:'memberEntityReference', E],[Element, bp:'entityFeature', B]].

hasBmParticipants(Ev, Part1, Part2) :-
    rdf(Ev, bmpaf:'hasParticipantA', Part1),
    rdf(Ev, bmpaf:'hasParticipantB', Part2).

bpBindingPattern(E1, E2, Uncert, Graph) :- 
    rdf(Complex, rdf:'type', bp:'Complex'),
    rdf(Complex, bp:'component', Component1),
    rdf(Component1, bp:'entityReference', E1),
    rdf(E1, bp:'entityFeature', B1),
    rdf(Complex, bp:'component', Component2),
    rdf(Component2, bp:'entityReference', E2),
    rdf(B1, bp:'bindsTo', B2),
    rdf(Complex, bp:'feature', B2),
    findall(C, rdf(Complex, bp:'component', C), L),
    length(L, No),
    Uncert is (1 / (No - 0.9999)),
    Graph = [
        [Complex, bp:'component', Component1],
        [Component1, bp:'entityReference', E1],
        [E1, bp:'entityFeature', B1],
        [Complex, bp:'component', Component2],
        [Component2, bp:'entityReference', E2],
        [B1, bp:'bindsTo', B2],
        [Complex, bp:'feature', B2]].
    
bpBindingPattern(E1, E2, Uncert, Graph) :- 
    rdf(Complex, rdf:'type', bp:'Complex'), 
    rdf(Complex, bp:'component', Component1), 
    rdf(Component1, bp:'entityReference', E1), 
    rdf(E1, bp:'entityFeature', B1), 
    rdf(Complex, bp:'component', Component2), 
    rdf(Component2, bp:'entityReference', Compound), 
    rdf(Compound, bp:'memberEntityReference', E2), 
    rdf(B1, bp:'bindsTo', B2), 
    rdf(Complex, bp:'feature', B2),
    findall(M, rdf(Compound, bp:'memberEntityReference', M), LM),
    findall(C, rdf(Complex, bp:'component', C), LC),
    length(LM, No1),
    length(LC, No2),
    Uncert is (1 / (No1 + No2 - 0.9999)),
    Graph = [
        [Complex, bp:'component', Component1],
        [Component1, bp:'entityReference', E1],
        [E1, bp:'entityFeature', B1],
        [Complex, bp:'component', Component2],
        [Component2, bp:'entityReference', Compound],
        [Compound, bp:'memberEntityReference', E2],
        [B1, bp:'bindsTo', B2],
        [Complex, bp:'feature', B2]].

    
hasMatch(BmPart, BpRef, 1) :-
    rdf(BmPart, bm:'hasExactMatch', BpRef).

hasMatch(BmPart, BpRef, 0.5) :-
    rdf(BmPart, bm:'hasPotentialMatch', BpRef).
    
mostProbableMatch(Ev, Graph, Uncert) :-
    match(Ev, _, _, Graph, _, Uncert),
    \+moreProbable(Ev, Uncert).

moreProbable(Ev, Uncert) :-
    match(Ev, _, _, _, _, Uncert2),
    Uncert2 > Uncert.
    
subsetMinimalAndMostProbable(Ev, Graph, Certainty) :-
    mostProbableMatch(Ev, Graph, Certainty),
    \+subsetSmaller(Ev, Graph).
        
subsetSmaller(Ev, Graph) :-
    mostProbableMatch(Ev, Graph2, _),
    \+ Graph = Graph2,
    subset(Graph2, Graph).
    
encodeMinimal(Ev) :-
    subsetMinimalAndMostProbable(Ev, Graph, Uncert),
    uuid(U), 
    atomic_list_concat(['rdfBag', '_', U], Subject),
    writeInTargetGraph(Subject, rdf:'type', rdf:'Bag'),
    createClassedObject('probability', bmp:'ProbabilityRelevantToAssemblyUncertainty', Prob),
    writeInTargetGraph(Subject, bmp:'hasAssemblyProbability', Prob),
    insertDataValue(Prob, bmp:'hasProbabilityLevel', Uncert),
    writeInTargetGraph(Ev, bm:'hasMatch', Subject),
    forall(member(Triple, Graph), reifyIntoBag(Triple, Subject)).

reifyIntoBag([Sub, Pred, Obj], Graph) :-
    uuid(U), 
    atomic_list_concat(['rdfTriple', '_', U], Subject),
    writeInTargetGraph(Subject, rdf:'type', rdf:'Statement'),
    writeInTargetGraph(Graph, rdfs:'member', Subject),
    writeInTargetGraph(Subject, rdf:'subject', Sub),
    writeInTargetGraph(Subject, rdf:'predicate', Pred),
    writeInTargetGraph(Subject, rdf:'object', Obj).
    
bpControl(ControllerRef, ControlledRef, ControlType, Uncertainty, Graph) :-
    rdf(X, bp:'controlType', ControlTypeLiteral),
    rdf_literal_value(ControlTypeLiteral, ControlType),
    rdf(X, bp:'controller', Controller),
    bpControlElement(Controller, ControllerRef, Unc1, Graph1),
    rdf(X, bp:'controlled', Controlled), 
    bpControlledElements(Controlled, ControlledRef, Unc2, Graph2),
    Uncertainty is Unc1 * Unc2,
    Graph4 = [
        [X, bp:'controlType', ControlTypeLiteral],
        [X, bp:'controller', Controller],
        [X, bp:'controlled', Controlled]],
    append(Graph1, Graph2, Graph3),
    append(Graph3, Graph4, Graph).
    
regulation('ACTIVATION', ['http://www.semanticweb.org/ontologies/2015/7/untitled-ontology-160#PositiveRegulation', 'http://www.semanticweb.org/ontologies/2015/7/untitled-ontology-160#IncreasesActivity']).
regulation('INHIBITION', ['http://www.semanticweb.org/ontologies/2015/7/untitled-ontology-160#NegativeRegulation', 'http://www.semanticweb.org/ontologies/2015/7/untitled-ontology-160#DecreasesActivity']).


bpControlElement(Element, E, 1, [[Element, bp:'entityReference', E]]) :-
    rdf(Element, bp:'entityReference', E).

bpControlElement(Element, E, 0.5, Graph) :-
    rdf(Element, bp:'component', Component),
    rdf(Component, bp:'entityReference', E),
    Graph = [[Element, bp:'component', Component], [Component, bp:'entityReference', E]].

bpControlElement(Element, E, 0.5, [[Element, bp:'memberEntityReference', E]]) :-
    rdf(Element, bp:'memberEntityReference', E).
    

%TemplateReaction

bpControlledElements(Element, ERef, 1, Graph) :-
    rdf(Element, bp:'participant', E),
    rdf(E, bp:'entityReference', ERef),
    Graph = [[Element, bp:'participant', E],
    [E, bp:'entityReference', ERef]].

%BiochemicalReaction

%ComplexAssembly

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
insertDataValue(Subject, Predicate, Value) :-
    makeLiteral(Value, Literal),
    writeInTargetGraph(Subject, Predicate, Literal).   
    
makeLiteral(@false, literal(lang(en, 'false'))) :- !.
makeLiteral(@true, literal(lang(en, 'true'))) :- !.
makeLiteral(X, literal(type('http://www.w3.org/2001/XMLSchema#decimal', X))) :- 
    number(X), X >= 0, \+X > 1, !.
makeLiteral(X, literal(type('http://www.w3.org/2001/XMLSchema#int', X))) :- 
    integer(X), !.
makeLiteral(Value, literal(lang(en, Value))) :- !.

createClassedObject(UriBase, Class, Subject) :- !,
    createFreshObject(UriBase, Subject),
    writeInTargetGraph(Subject, rdf:'type', Class).
    
createFreshObject(UriBase, Subject) :-
    uuid(U), !, 
    atomic_list_concat([UriBase, '_', U], Subject).

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


%%%%%%%%%%%%%%%%%
 
 
eventGraph(G) :-
    rdf(G, rdf:'type', rdf:'Bag'),
    rdf(G, bmp:'hasAssemblyProbability', Prob),
    rdf(Prob, bmp:'hasProbabilityLevel', Uncert),
    writeln(Uncert),
    findall(T, rdf(G, rdfs:'member', T), L),
    forall(member(T, L),
    presentTriple(T)).
    
presentTriple(T) :-
    rdf(T, rdf:'subject', Subject),
    rdf(T, rdf:'predicate', Predicate),
    rdf(T, rdf:'object', Object),
    iri_xml_namespace(Subject, _, SubjectLocal),
    iri_xml_namespace(Predicate, _, PredicateLocal),
    iri_xml_namespace(Object, _, ObjectLocal),
    writeln([SubjectLocal, PredicateLocal, ObjectLocal]).





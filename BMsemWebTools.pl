:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/sparql_client)).
:- use_module(library(http/json)).
:- use_module(library(http/http_digest)).
:- use_module(library(http/http_open)).

:- rdf_reset_db.

:- rdf_register_prefix(unprot, 'http://identifiers.org/uniprot/').
:- rdf_register_prefix(bm, 'http://purl.bioontology.org/net/brunel/bm/').
:- rdf_register_prefix(panda, 'http://purl.bioontology.org/net/brunel/panda#').
:- rdf_register_prefix(uno, 'http://purl.bioontology.org/net/brunel/uno#').
:- rdf_register_prefix(owl, 'http://www.w3.org/2002/07/owl#').
:- rdf_register_prefix(xsd, 'http://www.w3.org/2001/XMLSchema#').
:- rdf_register_prefix(rdfs, 'http://www.w3.org/2000/01/rdf-schema#').
:- rdf_register_prefix(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').
:- rdf_register_prefix(void, 'http://rdfs.org/ns/void#').
:- rdf_register_prefix(dc, 'http://purl.org/dc/elements/1.1/').
:- rdf_register_prefix(dct, 'http://purl.org/dc/terms/').
:- rdf_register_prefix(prov, 'http://www.w3.org/ns/prov-o#').

:- rdf_meta sparqlTerm(r, -).

:-[BMserverParams.pl].

panda_graph('http://purl.bioontology.org/net/brunel/panda').
event_graph('http://purl.bioontology.org/net/brunel/bm/event_graph').
submitter_graph('http://purl.bioontology.org/net/brunel/bm/submitter_graph').
source_graph('http://purl.bioontology.org/net/brunel/bm/source_graph').
journal_graph('http://purl.bioontology.org/net/brunel/bm/journal_graph_2015').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   RDF graph manager
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

saveGraph(Graph) :- 
    file_search_path(my_home, Dir),
    atomic_list_concat([Dir, '/', 'output', '.rdf'], OutputPath),
    rdf_save(OutputPath, [graph(Graph), encoding(utf8)]), !.

%pushGraph(RDFgraph) :-
%    findall([X, Y, Z], rdf(X, Y, Z, RDFgraph), GraphPattern),
%    sparqlInsertQuery(GraphPattern, RDFgraph),
%    rdf_retractall(_, _, _, RDFgraph).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SPARQL manager
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


askSparqlQuery(Query, Result) :-
    sparql_setup(Host, Port, Path, Author),
    sparql_query(Query, Result, [host(Host), port(Port), path(Path), status_code(_), Author]).

askHttpSparql(Query, Format, Result, Code) :-
    sparql_setup(Host, Port, Path, Author),
    http_open([ host(Host), port(Port), path(Path), search([query=Query, format=Format])], Result, [status_code(Code), Author]).

sparqlJsonQuery(Query, Tuple) :-
    askHttpSparql(Query, 'application/json', JsonStream, Code), 
    (\+Code = 200 -> (Tuple=[], atomic_list_concat(['Error: ', Code, ' on ', Query], ErrorMessage), writeln(ErrorMessage), true); 
    (json_read(JsonStream, json(Result)),
    member((results=json(X)),Result), 
    member((bindings=Z), X),
    member(json(JsonTup), Z),
    findall(Val, (member(_=json(Bind), JsonTup),
                  member(value=Val, Bind)), Tuple))).

sparqlInsertQuery(Graph, RDFgraph) :-  
    triplePatternGenerator(Graph, TriplePattern), !, 
    atomic_list_concat(['INSERT {GRAPH <', RDFgraph,'> {', TriplePattern, '}}'], Query),
%    log_writeln(''), log_writeln(Query),
    sparql_setup(Host, Port, Path, Author),
    http_open([host(Host), port(Port), path(Path), search([query=Query])], _, [Author]).
                  
sparqlSelectQuery(Graph, RDFgraph, Tuple) :-  
    triplePatternGenerator(Graph, TriplePattern), !, 
    atomic_list_concat(['SELECT * WHERE {GRAPH <', RDFgraph,'> {', TriplePattern, '}}'], Query),
%    log_writeln(''), log_writeln(Query),
    sparqlJsonQuery(Query, Tuple).
    
sparqlSelectQueryGlobal(Graph, QueryVariables, Tuple) :-  
    triplePatternGenerator(Graph, TriplePattern), !, 
    atomic_list_concat(['SELECT DISTINCT ', QueryVariables, ' WHERE {', TriplePattern, '}'], Query),
%    log_writeln(''), log_writeln(Query),
    sparqlJsonQuery(Query, Tuple).

sparqlClearGraph(Graph) :-
    atomic_list_concat(['CLEAR GRAPH <',Graph,'>'], Query),
    writeln(Query), 
    sparql_setup(Host, Port, Path, Author),
    http_open([host(Host), port(Port), path(Path), search([query=Query])], _, [status_code(_), Author]).

triplePatternGenerator(Graph, TriplePattern) :-
    tripleSetGenerator(Graph, TripleSet),   
    atomic_list_concat(TripleSet, '. ', TriplePattern).    
    
tripleSetGenerator([], []) :- !.

tripleSetGenerator([[X, Y, Z]|Rest], [Triple|RestTriples]) :-
    tripleSetGenerator(Rest, RestTriples),
    sparqlTerm(X, Xterm),
    sparqlTerm(Y, Yterm),
    sparqlTerm(Z, Zterm),
    atomic_list_concat([Xterm, Yterm, Zterm], ' ', Triple).

tripleSetGenerator([not(NotTriples)|Rest], [Triple|RestTriples]) :-
    tripleSetGenerator(Rest, RestTriples),
    triplePatternGenerator(NotTriples, NotPattern),
    atomic_list_concat(['filter not exists {', NotPattern, '}'], ' ', Triple).

substitute(_, _, [], []) :- !.

substitute(Var, Term, [OldTriple|OldRest], [NewTriple|NewRest]) :-
    \+OldTriple = not(_),
    (member(Var, OldTriple) -> select(Var, OldTriple, Term, NewTriple); NewTriple=OldTriple),
    substitute(Var, Term, OldRest, NewRest), !.  

substitute(Var, Term, [not(_)|OldRest], NewRest) :-
    substitute(Var, Term, OldRest, NewRest), !.
    
sparqlTerm(X, X) :-
    atomic(X),
    string_concat('?', _, X), !.

sparqlTerm(literal(X), Term) :-
    makeLiteral(X, Term), !.
    
sparqlTerm(X, Term) :-
    \+X = literal(_),
    rdf_global_id(X, URI),
    atomic_list_concat(['<', URI, '>'], Term), !.

makeLiteral(X, RDFLit) :- 
    atomic_list_concat([Y, M, D], '-', X), 
    atom_number(Y, Yn), atom_number(M, Mn), atom_number(D, Dn), 
    1900 < Yn, Yn < 2099, 0 < Mn, Mn < 13, 0< Dn, Dn <32, !,
    atomic_list_concat(['"', X, '"', '^^xsd:date'], RDFLit).
makeLiteral(X, RDFLit) :- 
    integer(X), !,
    atomic_list_concat(['"', X, '"', '^^xsd:integer'], RDFLit).
makeLiteral(X, RDFLit) :- 
    number(X), 
    \+integer(X), !,
    atomic_list_concat(['"', X, '"', '^^xsd:double'], RDFLit).
makeLiteral(X, RDFLit) :- 
    atomic_list_concat(['"', X, '"'], RDFLit), !.

%%%%%%%%%%%%%%%%%%%%%
% URI generation
%%%%%%%%%%%%%%%%%%%%%

createFreshObject(TypeUri, Subject) :-
    uuid(U), 
    rdf_current_prefix(_, Expansion),
    atom_concat(Expansion, Local, TypeUri), !,
    downcase_atom(Local, Name),
    atomic_list_concat(['http://purl.bioontology.org/net/brunel/bm/', Name, '_', U], Subject).

createFreshObject(Type, Subject) :-
    uuid(U), 
    downcase_atom(Type, Name),
    atomic_list_concat(['http://purl.bioontology.org/net/brunel/bm/', Name, '_', U], Subject).


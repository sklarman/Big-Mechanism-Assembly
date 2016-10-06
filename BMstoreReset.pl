:- ['BMsemWebTools.pl'].

todayReset :-
    date(date(Y, M, D)),
    atomic_list_concat([Y, M, D], '-', Date),
    clearGraphsByDate(Date),
    findall(_, resetGraph(_), _).

clearGraphsByDate(Date) :-
    findall(_, clearGraphByDate(Date), _).

clearGraphByDate(Date) :- 
    QueryTriples = [
            ['?graph', dc:'date', literal(Date)],
            ['?graph', rdf:'type', void:'Dataset']
       ], !,
   sparqlSelectQueryGlobal(QueryTriples, '?graph', [Graph]),
   sparqlClearGraph(Graph).

clearGraphByTitle(Title) :- 
    QueryTriples = [
            ['?graph', dc:'title', literal(Title)],
            ['?graph', rdf:'type', void:'Dataset']
       ], !,
   sparqlSelectQueryGlobal(QueryTriples, '?graph', [Graph]),
   sparqlClearGraph(Graph).
   

resetGraph(submitter) :-
    Graph = 'http://purl.bioontology.org/net/brunel/bm/submitter_graph',
    Triples = [
        [Graph, dc:'title', literal('Submitter data')],
        [Graph, dc:'description', literal('This dataset contains information about the agents submitting index cards to the system.')],
        ['http://purl.bioontology.org/net/brunel/bm/brunel', rdf:'type', panda:'Submitter'],
        ['http://purl.bioontology.org/net/brunel/bm/brunel', rdfs:'label', literal('Brunel')]
        ],        
    resetGraph(Graph, Triples).

resetGraph(event) :-
    Graph = 'http://purl.bioontology.org/net/brunel/bm/event_graph',
    Triples = [
        [Graph, dc:'title', literal('Biological event data')],
        [Graph, dc:'description', literal('This dataset contains generic descriptions of biological events (molecular interactions), which are referenced by the statements extracted from the literature.')]
        ],        
    resetGraph(Graph, Triples).
    
resetGraph(strings) :-
    Graph = 'http://purl.bioontology.org/net/brunel/bm/strings_db_graph',
    Triples = [
        [Graph, dc:'title', literal('STRING DB statements')],
        [Graph, dc:'description', literal('This dataset contains statements extracted from the database STRING DB (see: http://string-db.org/). The statements include protein-protein interactions in the human organism.')],
        ['http://purl.bioontology.org/net/brunel/bm/strings_db_provenance_prob', rdf:'type', uno:'UncertaintyRelevantToDocumentProvenance'],
        ['http://purl.bioontology.org/net/brunel/bm/strings_db_provenance_prob', uno:'hasUncertaintyLevel', literal(0.9)],
        ['http://purl.bioontology.org/net/brunel/bm/strings_db_provenance_prob', rdfs:'label', literal(0.9)]
        ],        
    resetGraph(Graph, Triples).
    
resetGraph(source) :-
    Graph = 'http://purl.bioontology.org/net/brunel/bm/source_graph',
    Triples = [
        [Graph, dc:'title', literal('Source information data')],
        [Graph, dc:'description', literal('This dataset contains information about journal articles, bioinformatics databases, and other sources from which statements might have been extracted.')],
        ['http://string-db.org/', rdf:'type', panda:'Database'],
        ['http://string-db.org/', rdfs:'label', literal('STRING DB')]
        ],        
    resetGraph(Graph, Triples).

resetGraph(Graph, Triples) :-
    %sparqlClearGraph(Graph),
    date(date(Y, M, D)),
    atomic_list_concat([Y, M, D], '-', Date),
    atomic_list_concat([Graph, '.rdf'], GraphDump),
    atomic_list_concat([Graph, '.graph'], GraphBrowse),
    GenericTriples = [
                [Graph, rdf:'type', void:'Dataset'],
                [Graph, rdf:'type', prov:'Entity'],
                [Graph, dc:'creator', literal('Big Mechanism')],
                [Graph, dc:'date', literal(Date)],
                [Graph, void:'dataDump', GraphDump],
                [Graph, void:'dataBrowse', GraphBrowse]
                ],
    union(GenericTriples, Triples, TargetTriples),
    sparqlInsertQuery(TargetTriples, Graph).
    


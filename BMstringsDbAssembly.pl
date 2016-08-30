:- ['BMsemWebTools.pl'].
:- ['BMeventAssembly.pl'].

:- dynamic mapping/3.
:- dynamic interaction/6.

strings2panda :-
    load,
    findall(_, iterator(1), _).

load :-
    csv_read_file('StringsToPANDA/uniprot.csv', Rows1, [functor(mapping)]), 
    findall(_, (
        member(mapping(X, Y, Z), Rows1),
        atomic_list_concat(['UNIPROT:', X], UniID),
        (\+mapping(UniID, Y, Z) -> assert(mapping(UniID, Y, Z)); true)
        ), _),
    csv_read_file('StringsToPANDA/action.tsv', Rows2, [functor(interaction)]), 
    findall(_, (
        member(interaction(X, Y, A, B, C, D), Rows2),
        string_concat("9606.", StrId1, X),
        atom_string(AtomId1, StrId1),
        string_concat("9606.", StrId2, Y),
        atom_string(AtomId2, StrId2),
        assert(interaction(AtomId1, AtomId2, A, B, C, D))
        ), _).
 
iterator(M) :- 
    interaction(Id1, Id2, Inter, Action, Dir, Score),
    Confidence is Score / 1000,
    mapping(UniId1, Id1, GroundP1),
    mapping(UniId2, Id2, GroundP2),
    gensym('', Counter), 
    atom_number(Counter, Num),
    Num >= M,
    (Dir = 1 -> (
            cardGenerator(UniId1, Id1, UniId2, Id2, Inter, Action, GroundP1, GroundP2, Confidence), 
            writeln([UniId1, Id1, UniId2, Id2, Inter, Action, GroundP1, GroundP2, Confidence])
            ); 
            (
            cardGenerator(UniId2, Id2, UniId1, Id1, Inter, Action, GroundP2, GroundP1, Confidence), 
            writeln([UniId2, Id2, UniId1, Id1, Inter, Action, GroundP2, GroundP1, Confidence])
            )),
    createRDFfromStrings, 
    retractall(card(_, _)).
    
cardGenerator(UniId1, Id1, UniId2, Id2, expression, '', GroundP1, GroundP2, Confidence) :-
    CardFacts = [
        card('/interaction/interaction_type', gene_expression),
        card('/interaction/negative_information', '@false'),
        card('/interaction/participant_a/entity_text', Id1),
        card('/interaction/participant_a/identifier', UniId1),
        card('/interaction/participant_a/entity_type', protein),
        card('/interaction/participant_a/grounding_score', GroundP1),
        card('/interaction/participant_b/interaction_type', increases),
        card('/interaction/participant_b/entity_type', event),
        card('/interaction/participant_b/participant_b/entity_text', Id2),
        card('/interaction/participant_b/participant_b/identifier', UniId2),
        card('/interaction/participant_b/participant_b/entity_type', protein),
        card('/interaction/participant_b/participant_b/grounding_score', GroundP2),
        card('/meta/confidence', Confidence)
        ],
    maplist(assert, CardFacts).
    
cardGenerator(UniId1, Id1, UniId2, Id2, expression, inhibition, GroundP1, GroundP2, Confidence)  :-
    CardFacts = [
        card('/interaction/interaction_type', gene_expression),
        card('/interaction/negative_information', '@false'),
        card('/interaction/participant_a/entity_text', Id1),
        card('/interaction/participant_a/identifier', UniId1),
        card('/interaction/participant_a/entity_type', protein),
        card('/interaction/participant_a/grounding_score', GroundP1),
        card('/interaction/participant_b/interaction_type', decreases),
        card('/interaction/participant_b/entity_type', event),
        card('/interaction/participant_b/participant_b/entity_text', Id2),
        card('/interaction/participant_b/participant_b/identifier', UniId2),
        card('/interaction/participant_b/participant_b/entity_type', protein),
        card('/interaction/participant_b/participant_b/grounding_score', GroundP2),
        card('/meta/confidence', Confidence)
        ],
    maplist(assert, CardFacts).

cardGenerator(UniId1, Id1, UniId2, Id2, activation, _, GroundP1, GroundP2, Confidence) :-
    CardFacts = [
        card('/interaction/interaction_type', increases),
        card('/interaction/negative_information', '@false'),
        card('/interaction/participant_a/entity_text', Id1),
        card('/interaction/participant_a/identifier', UniId1),
        card('/interaction/participant_a/entity_type', protein),
        card('/interaction/participant_a/grounding_score', GroundP1),
        card('/interaction/participant_b/entity_text', Id2),
        card('/interaction/participant_b/identifier', UniId2),
        card('/interaction/participant_b/entity_type', protein),
        card('/interaction/participant_b/grounding_score', GroundP2),
        card('/meta/confidence', Confidence)
        ],
    maplist(assert, CardFacts).


cardGenerator(UniId1, Id1, UniId2, Id2, inhibition, _, GroundP1, GroundP2, Confidence) :-
    CardFacts = [
        card('/interaction/interaction_type', decreases),
        card('/interaction/negative_information', '@false'),
        card('/interaction/participant_a/entity_text', Id1),
        card('/interaction/participant_a/identifier', UniId1),
        card('/interaction/participant_a/entity_type', protein),
        card('/interaction/participant_a/grounding_score', GroundP1),
        card('/interaction/participant_b/entity_text', Id2),
        card('/interaction/participant_b/identifier', UniId2),
        card('/interaction/participant_b/entity_type', protein),
        card('/interaction/participant_b/grounding_score', GroundP2),
        card('/meta/confidence', Confidence)
        ],
    maplist(assert, CardFacts).

cardGenerator(UniId1, Id1, UniId2, Id2, binding, _, GroundP1, GroundP2, Confidence) :-
    CardFacts = [
        card('/interaction/interaction_type', binds),
        card('/interaction/negative_information', '@false'),
        card('/interaction/participant_a/entity_text', Id1),
        card('/interaction/participant_a/identifier', UniId1),
        card('/interaction/participant_a/entity_type', protein),
        card('/interaction/participant_a/grounding_score', GroundP1),
        card('/interaction/participant_b/entity_text', Id2),
        card('/interaction/participant_b/identifier', UniId2),
        card('/interaction/participant_b/entity_type', protein),
        card('/interaction/participant_b/grounding_score', GroundP2),
        card('/meta/confidence', Confidence)
        ],
    maplist(assert, CardFacts).
    
cardGenerator(UniId1, Id1, UniId2, Id2, ptmod, _, GroundP1, GroundP2, Confidence) :-
    CardFacts = [
        card('/interaction/interaction_type', adds_modification),
        card('/interaction/modification_type', protein_modification),
        card('/interaction/negative_information', '@false'),
        card('/interaction/participant_a/entity_text', Id1),
        card('/interaction/participant_a/identifier', UniId1),
        card('/interaction/participant_a/entity_type', protein),
        card('/interaction/participant_a/grounding_score', GroundP1),
        card('/interaction/participant_b/entity_text', Id2),
        card('/interaction/participant_b/identifier', UniId2),
        card('/interaction/participant_b/entity_type', protein),
        card('/interaction/participant_b/grounding_score', GroundP2),
        card('/meta/confidence', Confidence)
        ],
    maplist(assert, CardFacts).


createRDFfromStrings :- 
    createIdObject(Event, panda:'Event', '/interaction', GroundingProbList),
    createFreshObject(panda:'Statement', Statement), !,
    event_graph(EventGraph),
    getLabel(Event, EventGraph, EventLabel),
    atomic_list_concat(['{', EventLabel, '} true in StringDB'], StatLabel),
    Triples = [
        [Statement, panda:'hasAnnotator', panda:'Computer'],
        [Statement, rdf:'type', panda:'Statement'],
        [Statement, rdf:'type', prov:'Entity'],
        [Statement, rdfs:'label', literal(StatLabel)],
        [Statement, panda:'hasTruthValue', literal(true)],
        [Statement, panda:'represents', Event],
        [Statement, panda:'hasSubmitter', 'http://purl.bioontology.org/net/brunel/bm/brunel'],
        [Event, panda:'isRepresentedBy', Statement],
        [Statement, panda:'isExtractedFrom', 'http://string-db.org/']
        ],
    sparqlInsertQuery(Triples, 'http://purl.bioontology.org/net/brunel/bm/strings_db_graph'), !,
    write('Created new statement: \t'), writeln(StatLabel),
    log_write('\nCreated: \t'), log_writeln(StatLabel),
    createInputUncertaintyGraph(Statement, ProbGraph), 
    writeln('Creating uncertainty input graph...'),
    uncertaintyTriples(Statement, GroundingProbList, TargetProbTriples),
    sparqlInsertQuery(TargetProbTriples, ProbGraph), !.

uncertaintyTriples(Statement, GroundingProbList, Triples) :-
    retriveKeyValue(extractionAccurracy, Accurracy),
    createFreshObject(uno:'AccuracyOfExtractionFromText', Subject1),
    TriplesAccurracy = [
        [Statement, uno:'hasExtractionAccurracy', Subject1],
        [Subject1, rdf:'type', uno:'AccuracyOfExtractionFromText'],
        [Subject1, uno:'hasUncertaintyLevel', literal(Accurracy)],
        [Subject1, rdfs:'label', literal(Accurracy)]
        ],
    listAverage(GroundingProbList, Prob),
    createFreshObject(uno:'UncertaintyRelevantToEntityGrounding', Subject2),
    TriplesGrounding = [
        [Statement, uno:'hasGroundingUncertainty', Subject2],
        [Subject2, rdf:'type', uno:'UncertaintyRelevantToEntityGrounding'],
        [Subject2, uno:'hasUncertaintyLevel', literal(Prob)],
        [Subject2, rdfs:'label', literal(Prob)]
        ],
    createFreshObject(uno:'UncertaintyRelevantToDocumentProvenance', Subject3),
    TriplesProv = [
        [Statement, uno:'hasProvenanceUncertainty', Subject3],
        [Subject3, rdf:'type', uno:'UncertaintyRelevantToDocumentProvenance'],
        [Subject3, uno:'hasUncertaintyLevel', literal(0.9)],
        [Subject3, rdfs:'label', literal(0.9)]
        ],
    union(TriplesAccurracy, TriplesGrounding, Triples1),
    union(Triples1, TriplesProv, Triples).

:- ['BMsemWebTools.pl'].
:- ['BMeventAssembly.pl'].

:- dynamic mapping/3.
:- dynamic interaction/5.
:- dynamic mnemonic/2.
:- dynamic protein/1.

strings2panda :-
    open('StringsToPANDA/StringEvents4.nt', write, EvStream),
    asserta(outStream(eventGraph, EvStream)),
    open('StringsToPANDA/StringStatements4.nt', write, StatStream),
    asserta(outStream(statementGraph, StatStream)),
    Triples = [
        ['http://purl.bioontology.org/net/brunel/bm/strings_db_graph', dc:'title', literal('STRING DB statements')],
        ['http://purl.bioontology.org/net/brunel/bm/strings_db_graph', dc:'description', literal('This dataset contains statements extracted from the database STRING DB (see: http://string-db.org/). The statements include protein-protein interactions in the human organism.')],
        ['http://purl.bioontology.org/net/brunel/bm/strings_db_provenance_prob', rdf:'type', uno:'UncertaintyRelevantToInformationProvenance'],
        ['http://purl.bioontology.org/net/brunel/bm/strings_db_provenance_prob', uno:'hasUncertaintyLevel', literal(0.9)],
        ['http://purl.bioontology.org/net/brunel/bm/strings_db_provenance_prob', rdfs:'label', literal(0.9)]
        ],        
    pushTriples(Triples, statementGraph),
    load,
    findall(_, iterator, _),
    close(EvStream),
    close(StatStream).

load :-
    csv_read_file('StringsToPANDA/uniprot.csv', Rows1), 
    findall(_, (
        member(row(X, Y, Z), Rows1),
        atomic_list_concat(['UNIPROT:', X], UniID),
        (\+mapping(UniID, Y, Z) -> assert(mapping(UniID, Y, Z)); true)
        ), _),
    csv_read_file('StringsToPANDA/uniprotMnemonic.csv', Rows), 
    findall(_, (
        member(row(X, Y), Rows),
        (\+mnemonic(X, Y) -> assert(mnemonic(X, Y)); true)
        ), _),
    csv_read_file('StringsToPANDA/action4.tsv', Rows2), 
    findall(_, (
        member(row(X, Y, A, B, Dir, D), Rows2),
        string_concat("9606.", StrId1, X),
        atom_string(AtomId1, StrId1),
        string_concat("9606.", StrId2, Y),
        atom_string(AtomId2, StrId2),
        (Dir = 1 -> 
            (\+interaction(AtomId1, AtomId2, A, B, D) -> assert(interaction(AtomId1, AtomId2, A, B, D));true);
            (\+interaction(AtomId2, AtomId1, A, B, D) -> assert(interaction(AtomId2, AtomId1, A, B, D));true))
        ), _).
 
iterator :- 
    interaction(Id1, Id2, Inter, Action, Score),
    Confidence is Score / 1000,
    mapping(UniId1, Id1, GroundP1),
    mapping(UniId2, Id2, GroundP2),
    tripleGenerator(UniId1, Id1, UniId2, Id2, Inter, Action, GroundP1, GroundP2, Confidence),
    print_count100.
    
    
tripleGenerator(UniId1, Id1, UniId2, Id2, expression, '', GroundP1, GroundP2, Confidence) :-
    entityTriples(UniId1, IdURI1, Id1, Name1, UniId2, IdURI2, Id2, Name2, EntityTriples),
    atomic_list_concat(['gene expression (', Name2, ')'], Ev2Lab),
    atomic_list_concat(['(', Name1, ') positive regulation (', Ev2Lab,')'], Ev1Lab),
    atomic_list_concat(['{', Ev1Lab, '} true in STRING-DB (by Brunel)'], StatLab),
    atomic_list_concat(['gene expression (', UniId2, ')'], Ev2URItext),
    atomic_list_concat(['(', UniId1, ') positive regulation (', Ev2URItext,')'], Ev1URItext),
    label2URI(Ev2URItext, URIev2),
    label2URI(Ev1URItext, URIev1),
    statementTriples(URIev1, StatLab, GroundP1, GroundP2, Confidence, StatTriples),
    EventTriples = [
        [URIev2, rdf:type, panda:'GeneExpression'],
        [URIev1, rdf:type, panda:'PositiveRegulation'],
        [URIev2, panda:'hasParticipantB', IdURI2],
        [URIev1, panda:'hasParticipantB', URIev2],
        [URIev1, panda:'hasParticipantA', IdURI1],
        [URIev2, rdf:type, panda:'Participant'],
        [URIev1, rdfs:label, literal(Ev1Lab)],
        [URIev2, rdfs:label, literal(Ev2Lab)]
        ],
    union(EntityTriples, EventTriples, EvTriples),
    pushTriples(EvTriples, eventGraph),
    pushTriples(StatTriples, statementGraph), !.

tripleGenerator(UniId1, Id1, UniId2, Id2, expression, inhibition, GroundP1, GroundP2, Confidence)  :-
    entityTriples(UniId1, IdURI1, Id1, Name1, UniId2, IdURI2, Id2, Name2, EntityTriples),
    atomic_list_concat(['gene expression (', Name2, ')'], Ev2Lab),
    atomic_list_concat(['(', Name1, ') negative regulation (', Ev2Lab,')'], Ev1Lab),
    atomic_list_concat(['{', Ev1Lab, '} true in STRING-DB (by Brunel)'], StatLab),
    atomic_list_concat(['gene expression (', UniId2, ')'], Ev2URItext),
    atomic_list_concat(['(', UniId1, ') negative regulation (', Ev2URItext,')'], Ev1URItext),
    label2URI(Ev2URItext, URIev2),
    label2URI(Ev1URItext, URIev1),
    statementTriples(URIev1, StatLab, GroundP1, GroundP2, Confidence, StatTriples),
    EventTriples = [
        [URIev2, rdf:type, panda:'GeneExpression'],
        [URIev1, rdf:type, panda:'NegativeRegulation'],
        [URIev2, panda:'hasParticipantB', IdURI2],
        [URIev1, panda:'hasParticipantB', URIev2],
        [URIev1, panda:'hasParticipantA', IdURI1],
        [URIev2, rdf:type, panda:'Participant'],
        [URIev1, rdfs:label, literal(Ev1Lab)],
        [URIev2, rdfs:label, literal(Ev2Lab)]
        ],
    union(EntityTriples, EventTriples, EvTriples),
    pushTriples(EvTriples, eventGraph),
    pushTriples(StatTriples, statementGraph), !.
    
    
tripleGenerator(UniId1, Id1, UniId2, Id2, activation, _, GroundP1, GroundP2, Confidence) :-
    entityTriples(UniId1, IdURI1, Id1, Name1, UniId2, IdURI2, Id2, Name2, EntityTriples),
    atomic_list_concat(['(', Name1, ') positive regulation (', Name2,')'], Ev1Lab),
    atomic_list_concat(['{', Ev1Lab, '} true in STRING-DB (by Brunel)'], StatLab),
    atomic_list_concat(['(', UniId1, ') positive regulation (', UniId2,')'], Ev1URItext),
    label2URI(Ev1URItext, URIev1),
    statementTriples(URIev1, StatLab, GroundP1, GroundP2, Confidence, StatTriples),
    EventTriples = [
        [URIev1, rdf:type, panda:'PositiveRegulation'],
        [URIev1, panda:'hasParticipantB', IdURI2],
        [URIev1, panda:'hasParticipantA', IdURI1],
        [URIev1, rdfs:label, literal(Ev1Lab)]
        ],
    union(EntityTriples, EventTriples, EvTriples),
    pushTriples(EvTriples, eventGraph),
    pushTriples(StatTriples, statementGraph), !.


tripleGenerator(UniId1, Id1, UniId2, Id2, inhibition, _, GroundP1, GroundP2, Confidence) :-
    entityTriples(UniId1, IdURI1, Id1, Name1, UniId2, IdURI2, Id2, Name2, EntityTriples),
    atomic_list_concat(['(', Name1, ') negative regulation (', Name2,')'], Ev1Lab),
    atomic_list_concat(['{', Ev1Lab, '} true in STRING-DB (by Brunel)'], StatLab),
    atomic_list_concat(['(', UniId1, ') negative regulation (', UniId2,')'], Ev1URItext),
    label2URI(Ev1URItext, URIev1),
    statementTriples(URIev1, StatLab, GroundP1, GroundP2, Confidence, StatTriples),
    EventTriples = [
        [URIev1, rdf:type, panda:'NegativeRegulation'],
        [URIev1, panda:'hasParticipantB', IdURI2],
        [URIev1, panda:'hasParticipantA', IdURI1],
        [URIev1, rdfs:label, literal(Ev1Lab)]
        ],
    union(EntityTriples, EventTriples, EvTriples),
    pushTriples(EvTriples, eventGraph),
    pushTriples(StatTriples, statementGraph), !.

tripleGenerator(UniId1, Id1, UniId2, Id2, binding, _, GroundP1, GroundP2, Confidence) :-
    entityTriples(UniId1, IdURI1, Id1, Name1, UniId2, IdURI2, Id2, Name2, EntityTriples),
    atomic_list_concat(['(', Name1, ') binding (', Name2,')'], Ev1Lab),
    atomic_list_concat(['{', Ev1Lab, '} true in STRING-DB (by Brunel)'], StatLab),
    atomic_list_concat(['(', UniId1, ') binding (', UniId2,')'], Ev1URItext),
    label2URI(Ev1URItext, URIev1),
    statementTriples(URIev1, StatLab, GroundP1, GroundP2, Confidence, StatTriples),
    EventTriples = [
        [URIev1, rdf:type, panda:'Binding'],
        [URIev1, panda:'hasParticipantB', IdURI2],
        [URIev1, panda:'hasParticipantA', IdURI1],
        [URIev1, rdfs:label, literal(Ev1Lab)]
        ],
    union(EntityTriples, EventTriples, EvTriples),
    pushTriples(EvTriples, eventGraph),
    pushTriples(StatTriples, statementGraph), !.
    
tripleGenerator(UniId1, Id1, UniId2, Id2, ptmod, _, GroundP1, GroundP2, Confidence) :-
    entityTriples(UniId1, IdURI1, Id1, Name1, UniId2, IdURI2, Id2, Name2, EntityTriples),
    atomic_list_concat(['protein modification (', Name2, ')'], Ev2Lab),
    atomic_list_concat(['(', Name1, ') adds modification (', Ev2Lab,')'], Ev1Lab),
    atomic_list_concat(['{', Ev1Lab, '} true in STRING-DB (by Brunel)'], StatLab),
    atomic_list_concat(['protein modification (', UniId2, ')'], Ev2URItext),
    atomic_list_concat(['(', UniId1, ') adds modification (', Ev2URItext,')'], Ev1URItext),
    label2URI(Ev2URItext, URIev2),
    label2URI(Ev1URItext, URIev1),
    statementTriples(URIev1, StatLab, GroundP1, GroundP2, Confidence, StatTriples),
    EventTriples = [
        [URIev2, rdf:type, panda:'ProteinModification'],
        [URIev1, rdf:type, panda:'AddsModification'],
        [URIev2, panda:'hasParticipantB', IdURI2],
        [URIev1, panda:'hasParticipantB', URIev2],
        [URIev1, panda:'hasParticipantA', IdURI1],
        [URIev2, rdf:type, panda:'Participant'],
        [URIev1, rdfs:label, literal(Ev1Lab)],
        [URIev2, rdfs:label, literal(Ev2Lab)]
        ],
    union(EntityTriples, EventTriples, EvTriples),
    pushTriples(EvTriples, eventGraph),
    pushTriples(StatTriples, statementGraph), !.

entityTriples(UniId1, IdURI1, Id1, Name1, UniId2, IdURI2, Id2, Name2, EntityTriples) :-
    entityTriples(UniId1, IdURI1, Name1, EntityTriples1),
    entityTriples(UniId2, IdURI2, Name2, EntityTriples2),
    EntityTriples3 = [
        [IdURI1, panda:hasEntityText, literal(Id1)],
        [IdURI2, panda:hasEntityText, literal(Id2)]
        ], 
    union(EntityTriples1, EntityTriples2, EntityTriples4),
    union(EntityTriples3, EntityTriples4, EntityTriples), !.
  
entityTriples(UniId1, IdURI1, Name1, []) :-
    protein(UniId1),
    mnemonic(UniId1, Name1),
    createIdURI(simple, UniId1, IdURI1st),
    atom_string(IdURI1, IdURI1st), !.
           
entityTriples(UniId1, IdURI1, Name1, EntityTriples1) :-
    \+protein(UniId1),
    mnemonic(UniId1, Name1),
    createIdURI(simple, UniId1, IdURI1st),
    atom_string(IdURI1, IdURI1st),
    EntityTriples1 = [
        [IdURI1, rdf:type, panda:'Protein'],
        [IdURI1, panda:hasEntityId, literal(UniId1)],
        [IdURI1, panda:hasEntityName, literal(Name1)],
        [IdURI1, rdf:type, panda:'Participant']
        ], 
    asserta(protein(UniId1)), !.
        


statementTriples(URIev1, StatLab, GroundP1, GroundP2, Confidence, StatTriples) :-
    createFreshObject(panda:'Statement', Statement),
    createFreshObject(uno:'AccuracyOfExtractionFromText', Subject1),
    listAverage([GroundP1, GroundP2], Prob),
    createFreshObject(uno:'UncertaintyRelevantToEntityGrounding', Subject2),
    StatTriples =
        [
        [Statement, panda:'hasAnnotator', panda:'Computer'],
        [Statement, rdf:'type', panda:'Statement'],
        [Statement, rdf:'type', prov:'Entity'],
        [Statement, rdfs:'label', literal(StatLab)],
        [Statement, panda:'hasTruthValue', literal(true)],
        [Statement, panda:'represents', URIev1],
        [URIev1, panda:'isRepresentedBy', Statement],
        [Statement, panda:'hasSubmitter', 'http://purl.bioontology.org/net/brunel/bm/brunel'],
        [Statement, panda:'isExtractedFrom', 'http://string-db.org/'],
        [Statement, uno:'hasBiologicalUncertainty', Subject1],
        [Subject1, rdf:'type', uno:'UncertaintyRelevantToBiology'],
        [Subject1, uno:'hasUncertaintyLevel', literal(Confidence)],
        [Subject1, rdfs:'label', literal(Confidence)],
        [Statement, uno:'hasGroundingUncertainty', Subject2],
        [Subject2, rdf:'type', uno:'UncertaintyRelevantToEntityGrounding'],
        [Subject2, uno:'hasUncertaintyLevel', literal(Prob)],
        [Subject2, rdfs:'label', literal(Prob)],
        [Statement, uno:'hasProvenanceUncertainty', 'http://purl.bioontology.org/net/brunel/bm/strings_db_provenance_prob']
        ], !.

    

        

:- ['BMsemWebTools.pl'].
:- ['BMeventAssemblyAlt.pl'].

:- dynamic mapping/3.
:- dynamic interaction/5.

geneexp2panda :-
    load, writeln('Loaded data!'),
    log_stream(Stream),
    openOutputs,
    findall(_, iterator, _),
    rdf_retractall(_, _, _),
    closeOutputs,
    close(Stream).

load :- 
    rdf_load('journal_graph_2015.rdf', [graph(journals)]),
    rdf_load('PANDA.OWL', [graph(panda)]),
    csv_read_file('NameTables/uniprot.csv', Rows1), 
    findall(_, (
        member(row(X, Y), Rows1),
        assert(name(X, Y))
        ), _),
    csv_read_file('NameTables/chebi.csv', Rows2), 
    findall(_, (
        member(row(X, Y), Rows2),
        assert(name(X, Y))
        ), _),
    csv_read_file('gene_expression_2016-8-5.csv', Rows3), 
    maplist(assert, Rows3).
 
iterator :- 
    cardGenerator(IndexId), 
    (createRDF(IndexId) -> (log_writeln('\nSuccess!\n\n'), writeln('Success!'));(log_writeln('\nFail!\n\n'), writeln('Fail!'), readln(_))),
    retractall(card(_, _)).
    
cardGenerator(IndexId) :-
    row(IndexId, IntType, gene_expression, gene, GeneText, GeneId, chemical, ChemText, ChemId, TextUnc, Conf, Evid, PMC),
    gensym('', Counter), 
    writeln(Counter),
%    writeln([IndexId, IntType, gene_expression, gene, GeneText, GeneId, chemical, ChemText, ChemId, TextUnc, Conf, Evid, PMC]), readln(_),
    (TextUnc = uncertain -> Unc = @true; Unc = @false),
    CardFacts = [
        card('/interaction/interaction_type', IntType),
        card('/interaction/negative_information', @false),
        card('/interaction/participant_a/entity_text', ChemText),
        card('/interaction/participant_a/identifier', ChemId),
        card('/interaction/participant_a/entity_type', chemical),
        card('/interaction/participant_b/interaction_type', gene_expression),
        card('/interaction/participant_b/entity_type', event),
        card('/interaction/participant_b/participant_b/entity_text', GeneText),
        card('/interaction/participant_b/participant_b/identifier', GeneId),
        card('/interaction/participant_b/participant_b/entity_type', gene),
        card('/meta/confidence', Conf),
        card('/meta/uncertain', Unc),
        card('/pmc_id', PMC),
        card('/submitter', 'NaCTeM'),
        card('/evidence', Evid)
        ],
    maplist(assert, CardFacts).
    


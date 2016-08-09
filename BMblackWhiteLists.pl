:- ['BMsemWebTools.pl'].
:- ['BMeventAssemblyAlt.pl'].

chemicalLists :- 
    csv_read_file('ChemicalLists/black.csv', Rows1), 
    findall(_, (
        member(row(X), Rows1),
	createIdURI(simple, X, IdURIst),
	atom_string(IdURI, IdURIst),
        rdf_assert(IdURI, rdf:'type', panda:'BlackListedChemical')
        ), _),
    csv_read_file('ChemicalLists/white.csv', Rows2), 
    findall(_, (
        member(row(X), Rows2),
        createIdURI(simple, X, IdURIst),
	atom_string(IdURI, IdURIst),
        rdf_assert(IdURI, rdf:'type', panda:'WhiteListedChemical')
        ), _).
 
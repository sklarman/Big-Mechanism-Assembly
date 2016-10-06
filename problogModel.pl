
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ProbLog model and parameters 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% registering given probability values and fixing defaults
% E.g. for extractionProb:
% extractionProb(x, 0).                                     -> a "dummy" fact required by problog to activate predicate extractionProb/2
% extractionProbDefined(Stat) :- extractionProb(Stat, _).   -> if extraction probabability for statement Stat is present in the data, make extractionProbDefined(Stat) true 
% P::extractionProb(Stat) :- extractionProb(Stat, P).       -> the probability of extractionProb(Stat) is P, as found in the data.
% 0.63::extractionProb(Stat) :- \+extractionProbDefined(Stat). -> if extraction probabability is not known fix the default at 0.63 (the average) of all known extraction probabilities on a sample of 12K statements. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

textProb(x, 0).
textProbDefined(Stat) :- textProb(Stat, _). 
1::textProb(Stat) :- textProb(Stat, certain).
0.8::textProb(Stat) :- textProb(Stat, uncertain).
0.9::textProb(Stat) :- \+textProbDefined(Stat).

extractionProb(x, 0).
extractionProbDefined(Stat) :- extractionProb(Stat, _). 
P::extractionProb(Stat) :- extractionProb(Stat, P).
0.63::extractionProb(Stat) :- \+extractionProbDefined(Stat).

provenanceProb(x, 0).
provenanceProbDefined(Source) :- provenanceProb(Source, _). 
P::provenanceProb(Source) :- provenanceProb(Source, P).
0.5::provenanceProb(Source) :- \+provenanceProbDefined(Source).

groundProb(x, 0).
groundProbDefined(Stat) :- groundProb(Stat, _). 
P::groundProb(Stat) :- groundProb(Stat, P).
0.5::groundProb(Stat) :- \+groundProbDefined(Stat).

biolProb(x, 0).
biolProbDefined(Stat) :- biolProb(Stat, _). 
P::biolProb(Stat) :- biolProb(Stat, P).
0.5::biolProb(Stat) :- \+biolProbDefined(Stat).

representingStatement(x, x, x, x, x).
somePositiveSupport(Ev) :- event(Ev), representingStatement(Ev, _, true, _, _).
someNegativeSupport(Ev) :- event(Ev), representingStatement(Ev, _, false, _, _).
0::positiveSupport(Ev) :- event(Ev), \+somePositiveSupport(Ev).
0::negativeSupport(Ev) :- event(Ev), \+someNegativeSupport(Ev).

experimentProb(x, 0).

trust(_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% defining "weights" of different probabilities, i.e., margins to which different probabilities might affect the overall score.
% E.g. the probability of textProbWeighted might vary between 1 and 0.76650623, so it won't affect the score as much as e.g. groundProbWeighted. 
% the lower bound has been derived via ProbLog learning given a set of inital experimental ground truth data. 
% In case of groundProbWeighted and biolProbWeighted it is only ensured that the probability is greater than 0. 
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


1::textProbWeighted(Stat) :- textProb(Stat).
0.76650623::textProbWeighted(Stat).
1::provenanceProbWeighted(Stat) :- provenanceProb(Stat).
0.38908585::provenanceProbWeighted(Stat).
1::extractionProbWeighted(Stat) :- extractionProb(Stat).
0.9::extractionProbWeighted(Stat).
1::groundProbWeighted(Stat) :- groundProb(Stat).
0.01::groundProbWeighted(Stat).
1::biolProbWeighted(Stat) :- biolProb(Stat).
0.01::biolProbWeighted(Stat).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% computing supports, inconsistency, likelihoods
%
% The weights over the rules reflect the basic strategy of managing the redundancy of statments supporting each event: 
% statements coming from the same document and the same submitter contribute less to the final score than statments 
% coming from different documents and submitters. 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

0.6::positiveStatementSupport(Ev, Source, Sub) :- 
        representingStatement(Ev, Stat, true, Source, Sub), 
        textProbWeighted(Stat), 
        extractionProbWeighted(Stat), 
        groundProbWeighted(Stat),
        biolProbWeighted(Stat). 
        
0.6::negativeStatementSupport(Ev, Source, Sub) :- 
        representingStatement(Ev, Stat, false, Source, Sub), 
        textProbWeighted(Stat), 
        extractionProbWeighted(Stat), 
        groundProbWeighted(Stat),
        biolProbWeighted(Stat). 
        
0.8::positiveSubmitterSupport(Ev, Source) :- 
        positiveStatementSupport(Ev, Source, Sub),
        trust(Sub). 
        
0.8::negativeSubmitterSupport(Ev, Source) :- 
        negativeStatementSupport(Ev, Source, Sub),
        trust(Sub). 

0.9::positiveInformationSourceSupport(Ev) :- 
        positiveSubmitterSupport(Ev, Source), 
        provenanceProbWeighted(Source). 
        
0.9::negativeInformationSourceSupport(Ev) :- 
        negativeSubmitterSupport(Ev, Source), 
        provenanceProbWeighted(Source).

positiveSupport(Ev) :-  
        positiveInformationSourceSupport(Ev).

0.8::positiveSupport(Ev) :-  
        experimentProb(Ev, true).

negativeSupport(Ev) :- 
        negativeInformationSourceSupport(Ev).
       
0.5::negativeSupport(Ev) :- 
        experimentProb(Ev, false).

internalInconsistency(Ev) :- 
        event(Ev),
        positiveSupport(Ev), 
        negativeSupport(Ev).

totalPositiveUncertainty(Ev) :- 
        event(Ev), 
        positiveSupport(Ev), 
        \+negativeSupport(Ev).
        
totalNegativeUncertainty(Ev) :- 
        event(Ev), 
        \+positiveSupport(Ev), 
        negativeSupport(Ev).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ProbLog query
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
    

query(positiveSupport(Ev)).
query(negativeSupport(Ev)).

query(internalInconsistency(Ev)).

query(totalPositiveUncertainty(Ev)).
query(totalNegativeUncertainty(Ev)).

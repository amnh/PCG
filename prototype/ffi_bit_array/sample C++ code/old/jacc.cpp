#include <iostream>
#include <map>
#include <fstream>
#include <set> 
using namespace std ;


int intersectsize (set<int>&  A, set<int>& B)
{
  int numInCommon =0;
  if (A.size()<B.size())
  {
    for (set<int>::iterator it=A.begin(); it!=A.end(); it++)
        numInCommon += (B.find(*it)!=B.end()) ? 1 : 0 ;
  }
  else
  {
    for (set<int>::iterator it=B.begin(); it!=B.end(); it++)
        numInCommon += (A.find(*it)!=A.end()) ? 1 : 0 ;
  }
  return numInCommon ;
}

int unionsize (set<int>&  A, set<int>& B)
{
  return (A.size()+B.size()) - intersectsize(A,B);
}

void load_LDIDs(istream& is, multimap<int,int>& M, set<int>& Gs)
{
  int grammID, sentID, garbage ;
  while (!is.eof())
    {
      is >> grammID ;
      is >> sentID  ;
      is >> garbage ;
      Gs.insert(grammID);
      M.insert(make_pair(grammID, sentID));
   }
  cout << "FINISHED loading LD_ID's " << endl ; 
}

int main()
{
  ifstream LDIDs("ld_ids.txt");
  if (!LDIDs) { cout << "File open No Good ..." << endl ; exit(1); }

  set<int> GrammSet;
  multimap <int,int> GrammSentMap ;
  load_LDIDs (LDIDs, GrammSentMap, GrammSet);

  set<int>::iterator si, sj, sk ;
 
  multimap <int, int>:: iterator lb, ub, l1it, l2it ; // upper bound, lower bound, language iterator (lit)

  set<int> L1, L2 ; // language 1, language 2

  int grammCount = 0;

  for (si=GrammSet.begin(); si!=GrammSet.end(); si++)
    {  // for every grammar (i.e. 3,072)

       // fill up  L1
      lb = GrammSentMap.lower_bound(*si); ub = GrammSentMap.upper_bound(*si);
      for (l1it=lb; l1it!=ub; l1it++){L1.insert( l1it->second ); }  // (*l1it).second
      
      // now for all remaining grammars, fill up L2
      sj = si;  // si is iterating over the set of grammars, sj now is 
      sj++;            // pointing to the grammar to the left of where si is pointing.

      grammCount++; cout << "In L1 loop, Working on grammar number:"<< grammCount  << endl ;
      
      for ( ; sj != GrammSet.end(); sj++)
      {
	  // fill up L2 (note we're using sj to iterate for the L2 languages
          lb = GrammSentMap.lower_bound(*sj); ub = GrammSentMap.upper_bound(*sj);
          for (l2it=lb; l2it!=ub; l2it++){L2.insert( l2it->second ); }  // (*lit).second
          
          // we now have L1 and L2 filled with sentences

          int intersectionSz = intersectsize(L1,L2);
          int unionSz        = unionsize(L1,L2) ;
          double jaccard   = double(intersectionSz) / double(unionSz) ;

          cout << "Jaccard: " << jaccard << endl ;

	  L2.clear();
      } // end of inside L2 loop
    
      L1.clear() ;      
    }  // end of outside L1 loop

} // end of main

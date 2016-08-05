#include "dynamic_character.h"

dynChar_t( size_t totalLen ) {
    character = vector(totalLen);
}

dynChar_t( size_t alphSize = 5, size_t numElems = 1, uint64_t vals = {0} ) {
    character = vector( bufferSize(alphSize, numElems) );
}

dynChar_t( size_t characterLen ) {

}


dynChar_t operator&( dynChar_t const &right ) const {
    dynChar_t holder = dynChar_t( right.end() - right.start() );
    for( size_t i = right.start(); i < right.end(); i++ ) {
        holder[i] = right.dynChar[i] & dynChar[i];
    }
    return holder;
 }

dynChar_t operator[]( size_t pos, size_t alphSize ) {
    dynChar_t holder;
    for( size_t i ; i < right.arrLen; i++ ) {
        holder[i] = right.dynChar[i] & dynChar[i];
    }
    return holder;

}

dynChar_t operator==( dynChar_t const &right ) {
    if( character.size() != right.character.size() ) {
        return false;
    }
    for( vector< uint64_t >::iterator lIter = character.begin(), lEnd = character.end(), 
                                      rIter = right.begin(), rEnd = right.character.end(); 
                                      lIter < lEnd;
                                      ++lIter, ++rIter ) {
        if( *rIter != *lIter) {
            return false;
        }
    }
    return true;
}

dynChar_t operator| ( dynChar_t const &right ) {
    for( vector< uint64_t >::iterator lIter = character.begin(), lEnd = character.end(), 
                                      rIter = right.begin(), rEnd = right.character.end(); 
                                      lIter < lEnd;
                                      ++lIter, ++rIter ) {
        if( *rIter != *lIter) {
            return false;
        }
    }
    return true;

}

dynChar_t operator^ ( dynChar_t const &right );


bool operator==( const dynChar_t& right ) const {
    return character == right.character;
}

void SetBit( const size_t k ) {
    arr[ k / WORD_WIDTH ] |= (CANONICAL_ONE << (k % WORD_WIDTH)); 
}
void ClearBit( const size_t k ) { 
    arr[ k / WORD_WIDTH ] &= ~(CANONICAL_ONE << (k % WORD_WIDTH)); 
}

uint64_t TestBit( const size_t k ) { 
    return arr[ k / WORD_WIDTH ] & (CANONICAL_ONE << (k % WORD_WIDTH)); 
}
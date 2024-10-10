/*--------------------------------------------------------------------------*/
/*----------------------------- File main.cpp ------------------------------*/
/*--------------------------------------------------------------------------*/
/** @file
 * Small main() for testing the "core" SMS++. It defines a fake empty :Block,
 * creates and destroys one. Nothing more than a compilation test.
 *
 * \author Antonio Frangioni \n
 *         Dipartimento di Informatica \n
 *         Universita' di Pisa \n
 *
 * \copyright &copy; by Antonio Frangioni
 */
/*--------------------------------------------------------------------------*/
/*------------------------------ INCLUDES ----------------------------------*/
/*--------------------------------------------------------------------------*/

#include <SMS++/Block.h>

/*--------------------------------------------------------------------------*/
/*-------------------------------- USING -----------------------------------*/
/*--------------------------------------------------------------------------*/

using namespace SMSpp_di_unipi_it;

/*--------------------------------------------------------------------------*/
/*------------------------------- CLASSES ----------------------------------*/
/*--------------------------------------------------------------------------*/

class FakeBlock : public Block {
 public:

 FakeBlock( Block * father = nullptr ) : Block( father ) {}

 virtual void load( std::istream &input ) override final {}

 private:

 SMSpp_insert_in_factory_h;
 };

/*--------------------------------------------------------------------------*/
/*----------------------------- STATIC MEMBERS -----------------------------*/
/*--------------------------------------------------------------------------*/

// register FakeBlock to the Block factory
SMSpp_insert_in_factory_cpp_1( FakeBlock );

/*--------------------------------------------------------------------------*/
/*--------------------------------- Main -----------------------------------*/
/*--------------------------------------------------------------------------*/

int main( void )
{
 FakeBlock fb;

 return( 0 );
 }

/*--------------------------------------------------------------------------*/
/*------------------------- End File main.cpp ------------------------------*/
/*--------------------------------------------------------------------------*/


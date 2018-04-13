#include <iostream>
#include <fstream>

#include "tree_resolver.hpp"

int main(int argc, char* argv[])
{
    if (argc != 2) {
        std::cout << "You included " << argc - 1 << " file names. One, and only one, is necessary." << std::endl;
        return 0;
    }
    std::string line, returned;
    std::ifstream myfile; // input filestream
    myfile.open(argv[1]);

    if (myfile.is_open()) {
        while (getline( myfile,line )) {
            if (line.length() != 0) {
                std::cout << "from file: " << line << std::endl;
                returned = resolve(line);
                std::cout << "returned:  " << returned << std::endl;
            }
        }
        myfile.close();
    }
    else  std::cout << "Unable to open file." << std::endl;

    return 0;
}


std::string resolve(std::string input)
{
    std::string output;
    output = input;

    std::cout << "input:     " << input << std::endl;
    std::cout << "output:    " << output << std::endl;

    return output;
}

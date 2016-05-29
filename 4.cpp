#include <cassert>
#include <iostream>
#include <string>
#include "md5.h"

std::string myInp = "iwrupvqb";
int len = myInp.length();

bool correct(std::string hash)
{
    return hash.substr(0, 6) == "000000";
}

int main()
{
    std::cout << md5("abcdef609043") << std::endl;

    assert(correct("00000000qwe"));
    assert(!correct("10000000ewq"));

    for (int i = 1;;i++)
    {
        std::string thisSeed = myInp + std::to_string(i);
        if (i % 100000 == 0)
        {
            std::cout << thisSeed << std::endl;
        }
        std::string thisHash = md5(thisSeed);
        if (correct(thisHash))
        {
            std::cout << "Answer is: " << i << std::endl;
            return 0;
        }
    }
    return 0;
}
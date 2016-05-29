#include <vector>
#include <string>
#include <iostream>
#include <algorithm>
#include <cassert>

using namespace std;

enum Action { On, Off, Toggle };

struct Box {
	int x1, x2;
	int y1, y2;
};

int side = 1000;
vector<vector<int>> lights(side);

void fill()
{
	for (int i = 0; i < lights.size(); i++) {
		lights[i] = vector<int>(side);
	}
}

void fillWithFalse()
{
	for (auto& lightArr : lights) {
		for (int& light : lightArr) {
			light = 0;
		}
	}
}

int count()
{
	int result = 0;
	for (auto& lightArr : lights) {
		for (int& light : lightArr) {
			result += light;
		}
	}
	return result;
}

int actOn(int light, Action act)
{
	if (act == On)
		return light+1;
	else if (act == Off)
		return max(light-1, 0);
	else
		return light+2;
}

void perform(Action act, Box box)
{
	for (int y = box.y1; y < box.y2; y++) {
		vector<int>& row = lights[y];
		for (int x = box.x1; x < box.x2; x++) {
			int newBrig = actOn(row[x], act);
			row[x] = newBrig;
		}
	}
}

bool isPrefix(string prefix, string subject)
{
	return prefix.length() < subject.length() &&
		mismatch(prefix.begin(), prefix.end(), subject.begin()).first == prefix.end();
}

Action actOf(string instruction)
{
	if (isPrefix("turn on", instruction)) {
		return On;
	}
	else if (isPrefix("turn off", instruction)) {
		return Off;
	}
	else {
		return Toggle;
	}
}

string boxStrOf(Action act, string instruction)
{
	switch (act)
	{
	case On:
		// removing "turn on ", 8 chars
		instruction.erase(0, 8);
		break;
	case Off:
		// removing "turn off ", 9 chars
		instruction.erase(0, 9);
		break;
	case Toggle:
		// removing "toggle ", 7 chars
		instruction.erase(0, 7);
		break;
	}
	int from = instruction.find(" ");
	int to = instruction.rfind(" ");
	instruction.erase(from, to - from);
	return instruction;
}

Box parseBox(string str)
{
	Box result = { 0,0,0,0 };
	int comma1 = str.find(",");
	int space = str.find(" "); 
	int comma2 = str.rfind(",");
	assert(0 < comma1);
	assert(comma1 < space);
	assert(space < comma2);
	result.x1 = stoi(str.substr(0, comma1));
	result.y1 = stoi(str.substr(comma1+1, space));
	result.x2 = stoi(str.substr(space+1, comma2))+1;
	result.y2 = stoi(str.substr(comma2+1))+1;

	assert(result.x1 >= 0);
	assert(result.x2 <= side);
	assert(result.y1 >= 0);
	assert(result.y2 <= side);

	return result;
}

void parse(string instruction)
{
	Action act = actOf(instruction);
	string boxStr = boxStrOf(act, instruction);
	Box actOn = parseBox(boxStr);
	perform(act, actOn);
}

int main()
{
	fill();
	fillWithFalse();
	string nextLine = "";
	while (getline(cin, nextLine))
	{
		parse(nextLine);
	}
	int c = count();
	cout << c << std::endl;
	system("pause");
}
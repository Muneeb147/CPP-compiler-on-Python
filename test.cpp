#include <iostream>
int sum (int a, int b)
{
	cout << "sum" <<a+b;
	return a+b;

}

int recursivesum(int a, int b){

	switch(a)
	{
		case -1:
		cout << "stoped" << endl;
		break;

		default:
		recursivesum(a - 1,b);
	}
	return a+b;
}
void mul(int a,int b)
{
	cout << "PRODUCT :" << a*b << endl;
}
void concat(string a , string b)
{
	cout << "concat :" << a+" "+b << endl;
}

int globalval  = 1;
int array[4] = {1,2,3,4};
array[2] = 100;
string strarray[3] = {"theory", "of" ,"Automata"};
double adc = 1;
int main()
{
	string c= "c++";
	string pr = "project";
	int n=0;
	globalval++;
	switch(globalval)
	{
		case 2:
		cout << "globalval :" << globalval << endl;
		break;
		case 3 :
		cout << "SORRY" << endl;
		break;

		default:
		cout << "THIS IS DEFAULT" << endl;

	}

	switch(c)
	{
		case "python":
		cout << c << endl;
		break;

		case "c++":
		cout << c << endl;
		break;

		default:
		cout << "NO LANGUAGE" << endl;
	}
	int num = 0;
	int limit = 5;
	do
	{ 
		int l = 0;
		do
		{
			cout << "nested while " << endl;
			l++;
		} while(l<=limit);

		cout << "DO WHILE WORKING" << endl;
		num++;
		cout << "NUM : " << num << endl;
	} while(num <= limit);

	cout << "array[2] :" << array[2] << endl;
	array[2] = 100;
	cout << "array[2] :" << array[++n] << endl;
	cout << "strarray[2] :" << strarray[2] << endl;
	
	int a = recursivesum(2,1);
	int mysum = sum(4,3);
	switch (mysum)
	{
		case 7:
		cout << " THE SUM IS: " << mysum<< endl;
		break;

		default:
		cout << "sum is not correct" << endl;
	}
	mul(0,8);
	concat(c,pr);
}
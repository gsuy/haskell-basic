import 'dart:io';

Future<int> A ()=> Future.delayed(Duration(microseconds: 1000),(){
  print('init A');
  return 100;
});

Future<int> B ()=> Future.delayed(Duration(microseconds: 20),(){
  print('init B');
  return 200;
});

Future<int> C ()=> Future.delayed(Duration(microseconds: 3),(){
  print('init B');
  return 300;
});

void main() async {
  print('main-1');
  var fA = await A();
  print('main-2');
  var fB = await B();
  print('main-3');
  var fC = await C();
  

  print('future A: $fA');
  
  print('future B-1: $fB');
  print('future C: $fC');
  print('future B-2: $fA');
  

}

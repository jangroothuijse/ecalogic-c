package example.jdt;

public class ArrayTest {

	public static void main(String args[]){

		
		// define your java int array
		int[] intArray = new int[] {1,2,3};
		//int[] intArray2 = new int[] {4,5,6,7,8,9,10,11};
		
		intArray[2] = 12;
		
		// return intArray;
		
		//print the java int array
		for (int i=0; i<intArray.length; i++)
		{
		  System.out.println(intArray[i]);
		}
	}	
		// Manually allocate differing size second dimensions. 
	private void multiDim(){
		int twoD[][] = new int[4][]; 
		twoD[0] = new int[1]; 
		twoD[1] = new int[2]; 
		twoD[2] = new int[3]; 
		twoD[3] = new int[4]; 
		int i, j, k = 0; 
		for(i=0; i<4; i++) 
		for(j=0; j<i+1; j++) { 
		twoD[i][j] = k; 
		k++; 
		} 
		for(i=0; i<4; i++) { 
		for(j=0; j<i+1; j++) 
		System.out.print(twoD[i][j] + " "); 
		System.out.println(); 
		} 
		
		
	 }
	
	

	 /**
	   * Create a String array, then populate the array, 
	   * and finally print each element in the int array.
	   */
	  private void stringArrayExample()
	  {
	    String[] stringArray = new String[3];
	    stringArray[0] = "a";
	    stringArray[1] = "b";
	    stringArray[2] = "c";
	    System.out.println("stringArray output");
	    for (int i=0; i<stringArray.length; i++)
	    {
	      System.out.println(stringArray[i]);
	    }
	  }
	  
	  /**
	   * Create a Java int array and populate it in one step.
	   * Then get the array length and print each element in the array.
	   */
	  private void intArrayExample2()
	  {
	    int[] intArray = new int[] {4,5,6,7,8};
	    System.out.println("intArray output (version 2)");
	    for (int i=0; i<intArray.length; i++)
	    {
	      System.out.println(intArray[i]);
	    } 
	  }


}

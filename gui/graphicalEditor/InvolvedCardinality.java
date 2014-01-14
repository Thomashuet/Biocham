package fr.inria.contraintes.biocham.graphicalEditor;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

public class InvolvedCardinality {

	String rule, baseRule;
	String startingReactant;
	int numOfProducts;
	HashMap<String,Integer> numOfInvolvedReactionsOfProducts;
	int maxNumberOfPrForInvReactionsOfProd;
	int step_up;
	
	public InvolvedCardinality(String b,String r, int numP,int maxNumIR,HashMap<String,Integer> numIRP){
		rule=b;
		startingReactant=r;
		numOfProducts=numP;
		numOfInvolvedReactionsOfProducts=numIRP;
		for( Iterator it=numOfInvolvedReactionsOfProducts.entrySet().iterator(); it.hasNext();)   {  
	         Map.Entry entry =(Map.Entry)it.next(); 
	         String key =(String)entry.getKey(); 
	         int value =(Integer)entry.getValue(); 
	         //System.out.println(key+": numOfInvolvedReactionsOfProducts="+value);
	        
		}
		maxNumberOfPrForInvReactionsOfProd=maxNumIR;		
	}

	public String getBaseRule() {
		return baseRule;
	}

	public void setBaseRule(String baseRule) {
		this.baseRule = baseRule;
	}

	public int getMaxNumberOfPrForInvReactionsOfProd() {
		return maxNumberOfPrForInvReactionsOfProd;
	}

	public void setMaxNumberOfPrForInvReactionsOfProd(
			int maxNumberOfPrForInvReactionsOfProd) {
		this.maxNumberOfPrForInvReactionsOfProd = maxNumberOfPrForInvReactionsOfProd;
	}

	public HashMap<String, Integer> getNumOfInvolvedReactionsOfProducts() {
		return numOfInvolvedReactionsOfProducts;
	}

	public void setNumOfInvolvedReactionsOfProducts(
			HashMap<String, Integer> numOfInvolvedReactionsOfProducts) {
		this.numOfInvolvedReactionsOfProducts = numOfInvolvedReactionsOfProducts;
	}

	public int getNumOfProducts() {
		return numOfProducts;
	}

	public void setNumOfProducts(int numOfProducts) {
		this.numOfProducts = numOfProducts;
	}

	public String getStartingReactant() {
		return startingReactant;
	}

	public void setStartingReactant(String startingReactant) {
		this.startingReactant = startingReactant;
	}

	public String getRule() {
		return rule;
	}

	public void setRule(String rule) {
		this.rule = rule;
	}

	public int getStep_up() {
		return step_up;
	}

	public void setStep_up(int step_up) {
		this.step_up = step_up;
	}
	
}

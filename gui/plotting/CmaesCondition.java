package fr.inria.contraintes.biocham.plotting;

public class CmaesCondition {

	
	
	String qfltlQuery;	
	String listOfVariables;
	String listOfValues;
	String lsitOfParametersModifications;
	
	
	
	public CmaesCondition(String query,String lVars,String lVals, String lModif){
		
		qfltlQuery=query;
		listOfVariables=lVars;
		listOfValues=lVals;
		lsitOfParametersModifications=lModif;
		
	}



	
	
	
	
	public String getListOfValues() {
		return listOfValues;
	}
	public void setListOfValues(String listOfValues) {
		this.listOfValues = listOfValues;
	}

	
	
	public String getListOfVariables() {
		return listOfVariables;
	}
	public void setListOfVariables(String listOfVariables) {
		this.listOfVariables = listOfVariables;
	}
	
	
	
	public String getLsitOfParametersModifications() {
		return lsitOfParametersModifications;
	}
	public void setLsitOfParametersModifications(
			String lsitOfParametersModifications) {
		this.lsitOfParametersModifications = lsitOfParametersModifications;
	}
	
	
	
	public String getQfltlQuery() {
		return qfltlQuery;
	}
	public void setQfltlQuery(String qfltlQuery) {
		this.qfltlQuery = qfltlQuery;
	}
	
	
}

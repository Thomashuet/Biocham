package fr.inria.contraintes.biocham.plotting;

import java.util.ArrayList;

public class CurveFit {

	String moleculeName;
	ArrayList<String> time,values,sdsValues,sdsVariables;
	
	public CurveFit(){
		time=new ArrayList<String>();
		values=new ArrayList<String>();
		sdsValues=new ArrayList<String>();
		sdsVariables=new ArrayList<String>();
	}

	public String getMoleculeName() {
		return moleculeName;
	}

	public void setMoleculeName(String moleculeName) {
		this.moleculeName = moleculeName;
	}

	public ArrayList<String> getSdsValues() {
		return sdsValues;
	}

	public void addSdValue(String sd){
		sdsValues.add(sd);
	}
	
	public ArrayList<String> getTime() {
		return time;
	}
	
	public void addTime(String t){
		time.add(t);
	}

	public ArrayList<String> getValues() {
		return values;
	}
	public void addValue(String v){
		values.add(v);
	}

	public ArrayList<String> getSdsVariables() {
		return sdsVariables;
	}
	public void addSdVariable(String sd){
		sdsVariables.add(sd);
	}
	
}

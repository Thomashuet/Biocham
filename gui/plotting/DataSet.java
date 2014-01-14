package fr.inria.contraintes.biocham.plotting;

import java.awt.Color;
import java.util.ArrayList;





public class DataSet {

	
	
	private String labelName;
	private String marks;
	private boolean connected;
	private ArrayList<String> xTicks;
	private ArrayList<String> yValues;
	private boolean errorBars;
	private ArrayList<String> valuesInPairs;
	private Color color;
	
	public DataSet(String title, ArrayList<String> xticks, ArrayList<String> yvalues){
		labelName=title;
		xTicks=xticks;
		yValues=yvalues;
		valuesInPairs=new ArrayList<String>();
		errorBars=false;
		color=Color.BLACK;
		marks="none";
		connected=true;
	}
	
	public DataSet(){
		
	}

	public ArrayList<String> getValuesInPairs(){
		if(valuesInPairs.size()==0){			
			ArrayList<String> valuesInPairs=new ArrayList<String>();		
			for(int i=0;i<xTicks.size();i++){
				if(isErrorBars()){
					//valuesInPairs.add(xticks.get(i)+","+yVals.get(i)+","+z1Vals.get(i)+","+z2Vals.get(i));
				}else{
					valuesInPairs.add(xTicks.get(i)+","+yValues.get(i));
				}
			}
			setValuesInPairs(valuesInPairs);
			
		}
		return valuesInPairs;
	}
	
	public void setValuesInPairs(ArrayList<String> vs){
		valuesInPairs=vs;		
	}
	
	
	public String getLabelName() {
		return labelName;
	}
	public void setLabelName(String labelName) {	
		this.labelName = labelName;
	}
	public ArrayList<String> getXTicks() {	
		return xTicks;
	}
	public void setXTicks(ArrayList<String> ticks) {
		xTicks = ticks;
	}
	public ArrayList<String> getYValues() {	
	
		return yValues;
	}
	public void setYValues(ArrayList<String> values) {
		yValues = values;
	}
	public boolean isErrorBars() {
		return errorBars;
	}
	public void setErrorBars(boolean errorBars) {
		this.errorBars = errorBars;
	}
	public Color getColor() {
		return color;
	}
	public void setColor(Color color) {
		this.color = color;
	}
	
	public String toString(){
		return labelName;
	}

	public String getMarks() {
		return marks;
	}

	public void setMarks(String marks) {
		this.marks = marks;
	}

	public boolean isConnected() {
		return connected;
	}

	public void setConnected(boolean connected) {
		this.connected = connected;
	}
	
	
	
	
	
}

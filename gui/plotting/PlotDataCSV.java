package fr.inria.contraintes.biocham.plotting;

import java.util.ArrayList;
import java.util.HashMap;

import javax.swing.SwingUtilities;

public class PlotDataCSV {
	
	
	
	
	private ArrayList<String> xTicks;
	private String xAxisLabel="";	
	private HashMap<String,ArrayList<String>> dataSets;
	private ArrayList<String> items;
	
	public PlotDataCSV(){
		
		
		xTicks=new ArrayList<String>();
		dataSets=new HashMap<String,ArrayList<String>>(); 
		items=new ArrayList<String>();
	}
	
	

	public HashMap<String, ArrayList<String>> getDataSets() {
		return dataSets;
	}
	public ArrayList<String> getItemsYValues(String item){	
		
		return dataSets.get(item);
	}
	public void setDataSets(HashMap<String, ArrayList<String>> dataSets) {
		this.dataSets = dataSets;
	}
	public void addTitle(String title){
		int cnt=0;
		boolean justOne=true;
		for(int j=0;j<items.size();j++){
			if(items.get(j).equals(title)){
				justOne=false;
			}else if(items.get(j).startsWith(title) && items.get(j).contains("-v")){
				cnt++;
			}
		}
		if(cnt==0 && !justOne){
			title+="-v1";
		}else if(cnt>0){
			cnt++;
			title+="-v"+cnt;
		}
		items.add(title);
	}
	public ArrayList<String> getItems() {
		return items;
	}
	public void setItems(ArrayList<String> items) {
		this.items = items;
	}
	public String getXAxisLabel() {
		return xAxisLabel;
	}
	public void setXAxisLabel(String axisLabel) {
		xAxisLabel = axisLabel;
	}
	public ArrayList<String> getXTicks() {
		
		return xTicks;
	}
	public void setXTicks(ArrayList<String> ticks) {
		xTicks = ticks;
	}



	public void disposeData() {
		
		xTicks.clear();
		xTicks=null;
		xAxisLabel=null;	
		dataSets.clear();
		dataSets=null;
		items.clear();
		items=null;
		
	}
	

}

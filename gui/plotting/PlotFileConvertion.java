package fr.inria.contraintes.biocham.plotting;
import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.parsers.ParserCSV;
import freemarker.template.TemplateException;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.StringTokenizer;

import javax.swing.JOptionPane;






public class PlotFileConvertion {

	
	
	
	String csvFile=null;
	ParserCSV csvParser;
	PlotDataCSV csvData;
	ArrayList<DataSet> dataSetsList;
	BiochamModel model;
	
	
	
	
	
	public PlotFileConvertion(){
		
		dataSetsList=new ArrayList<DataSet>();
		
	}
	
	
	
	
	public String fromPlotToPlt(String f,BiochamModel m){
		
		
		model=m;
		String convertedFile = null;
		String s = null;		
		
		try {				
		
			
			BufferedReader in = new BufferedReader(new FileReader(f));			
			ArrayList<String> dataSets = null,cvsFileNames=null,titles=null,columns=null;
			String temp=null;			
			int start=0;
			while ((s = in.readLine()) != null) {	
				if (s.startsWith("plot")) {					
					dataSets=new ArrayList<String>();
					String dsets=s.substring(5);	
					while(dsets.contains(", ")){
						int op=dsets.indexOf(", ");
						String dss=dsets.substring(start,op);					
						dataSets.add(dss);
					
						dsets=dsets.substring(op+2);
					}
									
					dataSets.add(dsets);
				
				}		
			}
		
			cvsFileNames=new ArrayList<String>();
			if(dataSets!=null){
				titles=new ArrayList<String>(dataSets.size());
				int size1=dataSets.size();
				for(int i=0;i<dataSets.size();i++){
					String datasetNm=dataSets.get(i);
					StringTokenizer st=new StringTokenizer(dataSets.get(i));
					while(st.hasMoreTokens()){
						temp=st.nextToken();					
						if(temp.contains(".csv")){
							String tempy=temp.substring(1,temp.length()-1);
							cvsFileNames.add(tempy);
						}
						else if(temp.contains(":") && !temp.contains("::")){
						
						}else if(temp.equals("title")){
							temp=st.nextToken();
							String nm=temp.substring(1,temp.length()-1);
							titles.add(nm);
							
						}					
					}								
				}
				int size2=titles.size();
				DataSet ds=null;
				csvParser=new ParserCSV();
				setCsvFile(cvsFileNames.get(0));
				csvData=csvParser.parseCSVTextFile(cvsFileNames.get(0));	
			
				setCsvData(csvData);
				for(int i=0;i<titles.size();i++){			
					int size=csvData.getXTicks().size();
				
					ds=new DataSet(titles.get(i),csvData.getXTicks(),csvData.getItemsYValues(titles.get(i)));
					addDataSet(dataSetsList,ds);
				}
				PltFile plt=new PltFile();			
				convertedFile=plt.create(dataSetsList,model);
			}else{
				 JOptionPane.showMessageDialog(BiochamMainFrame.frame, "You've given an incorrect input file.","Warning",JOptionPane.INFORMATION_MESSAGE);
			}
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		} catch (TemplateException e) {
			e.printStackTrace();
		}	    
		return convertedFile;
	}
	
	
	
	
	
	private ArrayList<DataSet> addDataSet(ArrayList<DataSet> dataSetsList, DataSet ds) {
		
		int cnt=0;
		boolean justOne=true;
		ArrayList<String> items=new ArrayList<String>();
		for(int i=0;i<dataSetsList.size();i++){
			items.add(dataSetsList.get(i).getLabelName());
		}
		String title=ds.getLabelName();
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
		ds.setLabelName(title);
		dataSetsList.add(ds);
		return dataSetsList;
	}

	
	
	
	
	
	
	
	
	
	
	
	
	
	public String fromPltToPlot(String f){		
		String convertedFile = null;
		return convertedFile;
	}
	public ParserCSV getCsvParser() {
		return csvParser;
	}
	public void setCsvParser(ParserCSV csvParser) {
		this.csvParser = csvParser;
	}
	public PlotDataCSV getCsvData() {
		return csvData;
	}
	public void setCsvData(PlotDataCSV csvData) {
		this.csvData = csvData;
	}
	public ArrayList<DataSet> getDataSetsList() {
		return dataSetsList;
	}
	public void setDataSetsList(ArrayList<DataSet> dataSetsList) {
		this.dataSetsList = dataSetsList;
	}
	public String getCsvFile() {
		return csvFile;
	}
	public void setCsvFile(String csvFile) {
		this.csvFile = csvFile;
	}
}

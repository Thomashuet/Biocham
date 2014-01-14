package fr.inria.contraintes.biocham.parsers;

import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.plotting.PlotDataCSV;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.StringTokenizer;
import javax.swing.JOptionPane;





public class ParserCSV {

	public PlotDataCSV parseCSVExcelFile(String f){
		
		FileInputStream is=null;
		 PlotDataCSV csv=null;
		 
			
			
		try {
			
			is=new FileInputStream(f);
			HSSFWorkbook wb=new HSSFWorkbook(is);
			HSSFSheet sheet=wb.getSheetAt(0);
			
			HSSFRow row=sheet.getRow(0);
			Iterator cells = row.cellIterator();
			int labelsNum=0;
            while( cells.hasNext() ) {
            	
            	
            	HSSFCell cell=(HSSFCell)cells.next();
            	
				if(cell.getCellType()==HSSFCell.CELL_TYPE_STRING){
						labelsNum++;
				}
            }
            String xAxisLabel="";
            ArrayList<String> xTicks=new ArrayList<String>();
            if(row.getCell(0).getCellType()==HSSFCell.CELL_TYPE_STRING){
            	xAxisLabel=row.getCell(0).getStringCellValue();
            	if(xAxisLabel.startsWith("#")){
            		xAxisLabel=xAxisLabel.substring(1);
            	}
            }
         
            int rowNum=sheet.getLastRowNum();
            for(int i=1;i<rowNum;i++){
            	if(sheet.getRow(i).getCell(0)!=null){
	            	if(sheet.getRow(i).getCell(0).getCellType()==HSSFCell.CELL_TYPE_NUMERIC){
	            		xTicks.add(String.valueOf(sheet.getRow(i).getCell(0).getNumericCellValue()));
	            	}
            	}else{
            		break;
            	}
            }
            
            ArrayList<String> dataSetNames=new ArrayList<String>();
            for(int i=1;i<labelsNum;i++){
            	if(sheet.getRow(0).getCell(i)!=null && sheet.getRow(0).getCell(i).getCellType()==HSSFCell.CELL_TYPE_STRING){
            		String tmp=sheet.getRow(0).getCell(i).getStringCellValue();
            		if(tmp.contains("[")){
            			tmp=tmp.substring(1,tmp.length()-1);
            		}
            		dataSetNames.add(i-1,tmp);
            		
            	}
            }
            
            HashMap<String,ArrayList<String>> dataSets=new HashMap<String,ArrayList<String>>();            
            ArrayList<String> dataSetValues;
         
            for(int i=0;i<dataSetNames.size();i++){//for each molecule,get the yvalues...
            
            	dataSetValues=new ArrayList<String>();
            	for(int j=1;j<rowNum;j++){
                	if(sheet.getRow(j).getCell(i+1)!=null && sheet.getRow(j).getCell(i+1).getCellType()==HSSFCell.CELL_TYPE_NUMERIC){
                		String tmp=String.valueOf(sheet.getRow(j).getCell(i+1).getNumericCellValue());                	
                		dataSetValues.add(tmp);
                	}
                }
            	dataSets.put(dataSetNames.get(i), dataSetValues);                    	
            }
                      
            is.close();            
            csv=new PlotDataCSV();
            csv.setDataSets(dataSets);
            csv.setItems(dataSetNames);
            csv.setXTicks(xTicks);
            csv.setXAxisLabel(xAxisLabel);
            
            
		}catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return csv;
	}
	
	public PlotDataCSV parseCSVTextFile(String f){;
		
		BufferedReader br=null;
		PlotDataCSV csv=null;
		boolean error=false;
		try {
			
			br = new BufferedReader(new FileReader(f));
			String line="";
			boolean firstRow=false;
			
			csv=new PlotDataCSV();		
			int times=-1;
			while((line=br.readLine())!=null){
				 
				try{
					int i=0;
					StringTokenizer st=new StringTokenizer(line);
			
						if(!firstRow){
							boolean xAxisLabelSet=false;
							while(st.hasMoreTokens()){
								if(!xAxisLabelSet){
									String ff=st.nextToken();
									csv.setXAxisLabel(ff);
									xAxisLabelSet=true;
								}						
								String s=st.nextToken();
								if(s.contains("[")){
									s=s.substring(1,s.length()-1);
								}
								csv.addTitle(s);
								csv.getDataSets().put(s, new ArrayList<String>());
							}
							firstRow=true; 
							
						}else{
							boolean xTicksSet=false;
							
							while(st.hasMoreTokens()){
								if(!xTicksSet){
									String ff=st.nextToken();
									csv.getXTicks().add(ff);
									xTicksSet=true;						
								}else{					
									String p=csv.getItems().get(i);
									String ff=st.nextToken();
									csv.getDataSets().get(p).add(ff);
									i++;
								}
							}										
						}
				}catch(Exception e){
					
					error=true;
					br.close();
					e.printStackTrace();
					break;					
				}
			}	
			if(times>0){
				int time=times-1;
				int x=time/5;						
				if(time-5*x>0){
					time+=(5-(time-5*x));
				}				
				for(int j=0;j<time/5+1;j++){
					csv.getXTicks().add(""+j*5);					
				}
				
			}
			br.close();
			
		} catch (FileNotFoundException e) {
			JOptionPane.showMessageDialog(BiochamMainFrame.frame, "The input(plot file) is incorrect","Warning",JOptionPane.WARNING_MESSAGE);
			e.printStackTrace();
		} catch (IOException e) {
			JOptionPane.showMessageDialog(BiochamMainFrame.frame, "Reading input file error.","Warning",JOptionPane.WARNING_MESSAGE);
			e.printStackTrace();
		}
		if(error){
			JOptionPane.showMessageDialog(BiochamMainFrame.frame, "Reading input file error.","Warning",JOptionPane.WARNING_MESSAGE);
			return null;
		}else{
			return csv;
		}
		
	}
	

	
}

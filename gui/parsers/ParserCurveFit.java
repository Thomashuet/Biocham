package fr.inria.contraintes.biocham.parsers;

import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.plotting.CurveFit;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.StringTokenizer;

import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;



public class ParserCurveFit {

	

	File file=null;
	InputStream is=null;
	ArrayList<CurveFit> curveFits=new ArrayList<CurveFit>();
	private ArrayList<String> ltlQueries;
	
	public ArrayList<CurveFit> getCurveFits(File f){

		boolean b=SwingUtilities.isEventDispatchThread();
		
		
		String content="";	
		setLtlQueries(new ArrayList<String>());
		curveFits.clear();
		try {
			
			is=new FileInputStream(f);
			HSSFWorkbook wb=new HSSFWorkbook(is);
			HSSFSheet sheet=wb.getSheetAt(0);
			
			HSSFRow row=sheet.getRow(0);
			
			int moleculesNum=0;
			int cellsNum=row.getLastCellNum();
			int div1=cellsNum%2;
			int div2=cellsNum%3;
			int div=1;
			if(div1==0 && div2==0){
				div=3;
			}else if(div2==0){
				div=3;
			}else{
				div=2;
			}
			for(int i=0;i<cellsNum/div;i++){
				if(row.getCell(div*i+1)!=null){
					if(row.getCell(div*i+1).getCellType()==HSSFCell.CELL_TYPE_STRING){
						moleculesNum++;
					}
				}
			}
            for(int i=0;i<moleculesNum;i++){
            	
            	CurveFit cf=new CurveFit();
            	String name=sheet.getRow(0).getCell(div*i+1).getStringCellValue();     
            	StringTokenizer st=new StringTokenizer(name);
            	String rightName="";
            	while(st.hasMoreTokens()){
            		rightName+=st.nextToken();
            	}
            	
            	cf.setMoleculeName(rightName);
            	
            	int rows=sheet.getLastRowNum();
            	int countRows=0;
            	for(int j=1;j<rows;j++){
            		if(sheet.getRow(j).getCell(div*i)!=null && (sheet.getRow(j).getCell(div*i).getCellType()==HSSFCell.CELL_TYPE_NUMERIC ||sheet.getRow(j).getCell(div*i).getCellType()==HSSFCell.CELL_TYPE_STRING)){            			
            			countRows++;
            		}
            	}
            	for(int j=1;j<=countRows;j++){
            		
            		try{
	            		if(sheet.getRow(j).getCell(div*i)==null){
	            			break;
	            		}
		        		String t=String.valueOf(sheet.getRow(j).getCell(div*i).getNumericCellValue());
		        		cf.addTime(t);        		
		        		String sd=String.valueOf(sheet.getRow(j).getCell(div*i+1).getNumericCellValue());
		        		cf.addSdValue(sd);
		        		
		        		
            		}catch(IllegalStateException ex){
            			JOptionPane.showMessageDialog(BiochamMainFrame.frame, "You have an error in your excel file. Check if the values don't contain any characters.","Attention",JOptionPane.WARNING_MESSAGE);
            			return null;
            		}
	        		
            	}
            	curveFits.add(cf);
            }
           
            for(int i=0;i<curveFits.size();i++){
            	
            	
            	String molList="",timeList="",sdList=""; 
            	CurveFit cf=curveFits.get(i);
            	molList+=cf.getMoleculeName();
            	for(int j=0;j<cf.getTime().size();j++){
            		
            		
            		timeList+=cf.getTime().get(j);
            		sdList+="v"+i+j;     		
            		if((j+1)<cf.getTime().size()){
            			//molList+=",";
            			timeList+=",";
            			sdList+=",";
            		}
            	}            
            	cf.addSdVariable(sdList);
            	content+=" curve_fit("+molList+",["+timeList+"],["+sdList+"]) ";
            	getLtlQueries().add("curve_fit("+molList+",["+timeList+"],["+sdList+"])");
            	            	
            }
        				
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return curveFits;
		
	
	}
	public ArrayList<CurveFit> getCurveFitErrs(File f){
		
		ArrayList<String> content=new ArrayList<String>();	
		
		curveFits=new ArrayList<CurveFit>();
		content.clear();	
		setLtlQueries(new ArrayList<String>());
		curveFits.clear();
		try {
			
			is=new FileInputStream(f);
			HSSFWorkbook wb=new HSSFWorkbook(is);
			HSSFSheet sheet=wb.getSheetAt(0);
			
			HSSFRow row=sheet.getRow(0);
			Iterator cells = row.cellIterator();
			int moleculesNum=0;
            while( cells.hasNext() ) {
            	
            	
            	HSSFCell cell=(HSSFCell)cells.next();
            	
				if(cell.getCellType()==HSSFCell.CELL_TYPE_STRING){
						moleculesNum++;
				}
            }
            int curveFitsNum=moleculesNum/3;
            for(int i=0;i<curveFitsNum;i++){
            	CurveFit cf=new CurveFit();
            	String name=sheet.getRow(0).getCell(3*i+1).getStringCellValue();            	
        
            	cf.setMoleculeName(name);
            	
            	int rows=sheet.getLastRowNum();
            	for(int j=1;j<=rows;j++){
            		
            		if(sheet.getRow(j).getCell(3*i)==null){
            			break;
            		}
	        		String t=String.valueOf(sheet.getRow(j).getCell(3*i).getNumericCellValue());
	        		cf.addTime(t);
	        		
	        		String v=String.valueOf(sheet.getRow(j).getCell(3*i+1).getNumericCellValue());
	        		cf.addValue(v);
	        		
	        		String sd=String.valueOf(sheet.getRow(j).getCell(3*i+2).getNumericCellValue());
	        		cf.addSdValue(sd);
	        	
	        		
            	}
            	curveFits.add(cf);
            }
           
          
            for(int i=0;i<curveFits.size();i++){
            	
            	
            	String molList="",valuesList="",sdList="",timeList=""; 
            	CurveFit cf=curveFits.get(i);
            	molList+=cf.getMoleculeName();
            	
            	for(int j=0;j<cf.getTime().size();j++){
            		            		
            		sdList+="v"+i+j;   
            		valuesList+=cf.getValues().get(j);
            		timeList+=cf.getTime().get(j);
             		
            		if((j+1)<cf.getTime().size()){
            		
            			valuesList+=",";
            			timeList+=",";
            			sdList+=",";
            		}
            	}  
               	cf.addSdVariable(sdList);
            	content.add("curve_fit_error("+molList+",["+valuesList+"],["+timeList+"],["+sdList+"])");
            	getLtlQueries().add("curve_fit_error("+molList+",["+valuesList+"],["+timeList+"],["+sdList+"])");
            	            	
            }			
			
			
		} catch (FileNotFoundException e) {
			
			e.printStackTrace();
		} catch (IOException e) {
			
			e.printStackTrace();
		}
		return curveFits;
		
	}
	public ArrayList<CurveFit> getCurveFits() {
		return curveFits;
	}
	public void setLtlQueries(ArrayList<String> ltlQueries) {
		this.ltlQueries = ltlQueries;
	}
	public ArrayList<String> getLtlQueries() {
		return ltlQueries;
	}
}









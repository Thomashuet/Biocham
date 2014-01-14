package fr.inria.contraintes.biocham.utils;

import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;

import java.io.FileInputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.List;

import javax.swing.SwingUtilities;

public class DisplayExcelContent {
	
	private String content;
	
	
	public DisplayExcelContent(String filename){
		
		boolean b=SwingUtilities.isEventDispatchThread();
		
		
		//List sheetData = new ArrayList();
        FileInputStream fis = null;
        StringBuffer sb=new StringBuffer();
        try {
            //
            // Create a FileInputStream that will be use to read the excel file.
            //
            fis = new FileInputStream(filename);
            //
            // Create an excel workbook from the file system.
            //
            HSSFWorkbook workbook = new HSSFWorkbook(fis);
            //
            // Get the first sheet on the workbook.
            //
            HSSFSheet sheet = workbook.getSheetAt(0);
            //
            // When we have a sheet object in hand we can iterator on each
            // sheet's rows and on each row's cells. We store the data read
            // on an ArrayList so that we can printed the content of the excel
            // to the console.
            //
            Iterator rows = sheet.rowIterator();
            while (rows.hasNext()) {
                HSSFRow row = (HSSFRow) rows.next();
                Iterator cells = row.cellIterator();
              //  List data = new ArrayList();
                while (cells.hasNext()) {
                    HSSFCell cell = (HSSFCell) cells.next();
                    if(cell.getCellType()==HSSFCell.CELL_TYPE_NUMERIC){
                    	sb.append(cell.getNumericCellValue()+" ");
                    }else{
                    	sb.append(cell.getRichStringCellValue().getString()+" ");
                    }
                }
                sb.append("\n");
            }
        } catch (IOException e) {
        	e.printStackTrace();
        } finally {
        	if (fis != null) {
        		try {
					fis.close();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
        	}
        }
        setContent(sb.toString());
		
	}

   
    private String showExelData(List sheetData) {
        //
        // Iterates the data and print it out to the console.
        //
    	
    	boolean b=SwingUtilities.isEventDispatchThread();
    	
    	
    	StringBuffer sb=new StringBuffer();
        for (int i = 0; i < sheetData.size(); i++) {
            List list = (List) sheetData.get(i);
            for (int j = 0; j < list.size(); j++) {
                HSSFCell cell = (HSSFCell) list.get(j);
                if(cell.getCellType()==HSSFCell.CELL_TYPE_NUMERIC){
                	sb.append(cell.getNumericCellValue()+" ");
                }else{
                	sb.append(cell.getRichStringCellValue().getString()+" ");
                }
            }
            sb.append(" ");
        
        }
        return sb.toString();
    }


	public String getContent() {
		return content;
	}


	public void setContent(String content) {
		this.content = content;
	}
}

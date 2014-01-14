package fr.inria.contraintes.biocham.plotting;

import org.apache.poi.hssf.usermodel.HSSFCell;
import org.apache.poi.hssf.usermodel.HSSFRow;
import org.apache.poi.hssf.usermodel.HSSFSheet;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.customComponents.BiochamPlot;
import fr.inria.contraintes.biocham.customComponents.InfoToolTip;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.util.StringTokenizer;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.JToolTip;
import javax.swing.SpringLayout;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;




public class DatasetPropertiesPanel {

	
	JPanel panel;
	JTextField tfValues,nameText;
	final ButtonGroup group=new ButtonGroup();
	Color dsColor=null;
	BiochamModel model;
	JCheckBox impulses,bars,lines;
	JButton colorButton, addYValues;	
	JRadioButton rb1,rb2,rb3,rb4,rb5,rb6,
				 rb7,rb8,rb9,rb10,rb11,rb12,
				 rb13,rb14,rb15;
	DatasetProperties datasetProperties;
	JLabel b2;
	
	public DatasetProperties getDatasetProperties(boolean inputValidation) {
		
		
		boolean b=SwingUtilities.isEventDispatchThread();
		
		
		datasetProperties=new DatasetProperties();
		datasetProperties.setDatasetName(nameText.getText());
		if(dsColor==null){
			dsColor=Color.GREEN;
		}
		datasetProperties.setColor(dsColor);
		final String values=tfValues.getText();
		if(inputValidation){
			
			int correctness=checkValuesCorrectness(values);
			if(correctness>=0){
				datasetProperties.setValues(tfValues.getText());// 1,2,3;4,5,6;7,8,9; //or, 1,2;3,4;	
			}else{
				JOptionPane.showMessageDialog(BiochamMainFrame.frame, "The values of the dataset are not correct. Consult the info assistance for its correct format and try again.","Wrong Input",JOptionPane.ERROR_MESSAGE);
				return null;
			}
		}else{
			datasetProperties.setValues(tfValues.getText());
		}
		 
		if(rb1.isSelected()){
			datasetProperties.setMarks("none");
		}else if(rb2.isSelected()){
			datasetProperties.setMarks("points");
		}else if(rb3.isSelected()){
			datasetProperties.setMarks("dots");
		}else if(rb4.isSelected()){
			datasetProperties.setMarks("pixels");
		}else if(rb5.isSelected()){
			datasetProperties.setMarks("crosses");
		}else if(rb6.isSelected()){
			datasetProperties.setMarks("errorBars");
		}else if(rb7.isSelected()){
			datasetProperties.setMarks("circle");
		}else if(rb8.isSelected()){
			datasetProperties.setMarks("filled_dircle");
		}else if(rb9.isSelected()){
			datasetProperties.setMarks("square");
		}else if(rb10.isSelected()){
			datasetProperties.setMarks("filled_square");
		}else if(rb11.isSelected()){
			datasetProperties.setMarks("diamond");
		}else if(rb12.isSelected()){
			datasetProperties.setMarks("filled_diamond");
		}else if(rb13.isSelected()){
			datasetProperties.setMarks("triangle");
		}else if(rb14.isSelected()){
			datasetProperties.setMarks("filled_triangle");
		}else if(rb15.isSelected()){
			datasetProperties.setMarks("plus_sign");
		}else{
			datasetProperties.setMarks("dots");
		}
		if(impulses.isSelected()){
			datasetProperties.setImpulses(true);
		}else{
			datasetProperties.setImpulses(false);
		}
		if(bars.isSelected()){
			datasetProperties.setBars(true);
		}else{
			datasetProperties.setBars(false);
		}
		if(lines.isSelected()){
			datasetProperties.setLines(true);
		}else{
			datasetProperties.setLines(false);
		}
		
		return datasetProperties;
	}
	
	
    
	private int checkValuesCorrectness(String values) {

		
		StringTokenizer st=new StringTokenizer(values,";");
		int v1=st.countTokens();
		for(int i=0;i<v1;i++){
			String s=st.nextToken();
			StringTokenizer st1=new StringTokenizer(s,",");
			int v2=st1.countTokens();
			if(v2!=2 && v2!=3){
				return -1;
			}else{
				for(int y=0;y<v2;y++){
					String h=st1.nextToken();
					try{
						Double.valueOf(h);
					}catch(Exception e){
						return -1;
					}
				}
				
			}
		}
		return 1;
	}



	public DatasetPropertiesPanel(BiochamModel m){
		
		
		boolean b=SwingUtilities.isEventDispatchThread();
		
		
		model=m;		
		SpringLayout sl=new SpringLayout();
		panel=new JPanel(sl);
		JLabel values=new JLabel("Values:");
		JLabel name=new JLabel("Name:");
		nameText=new JTextField();
		nameText.setColumns(21);
		tfValues=new JTextField();
		tfValues.setEditable(false);
		tfValues.setColumns(23);
		addYValues=new JButton("Load");
		addYValues.setVisible(false);
		addYValues.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				
				Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
				String rep=Utils.showOpenDialog(BiochamMainFrame.frame,"dataset");			
				if (rep!=null) {
					final File file=new File(rep);
					if(!file.isDirectory()){
			            SwingWorker sw=new SwingWorker(){
			            	 String content=null;
							@Override
							public Object construct() {
								 content=parseCSVFile(file);
								return null;
							}
	
							@Override
							public void finished() {
								boolean b=SwingUtilities.isEventDispatchThread();	
								 tfValues.setText(content);
								
							}};
							sw.start();
					}else{
						JOptionPane.showMessageDialog(BiochamMainFrame.frame,"Select a file, not a whole directory!", "Selection Warning",JOptionPane.WARNING_MESSAGE);
					}
		           
				}
				
				
			}});	
		b2=new JLabel(Icons.icons.get("infoAssist.png"+0.7)){
			public JToolTip createToolTip() {
				return new InfoToolTip("projectImages/exmpl.png");
			}
		};    	 
		b2.setToolTipText("");	    	 
		b2.addMouseListener(new MouseAdapter(){
			public void mouseClicked(MouseEvent e) {
				ToolTipManager.sharedInstance().setInitialDelay(0);    	    		
				ToolTipManager.sharedInstance().setEnabled(true);
			}
		});	
		b2.setVisible(false);
		
		panel.add(name);
		panel.add(nameText);
		panel.add(values);
		panel.add(tfValues);
		panel.add(addYValues);
		panel.add(b2);
		
		JLabel marks=new JLabel("Marks:");
		rb1=new JRadioButton("none");	
		rb1.setSelected(true);
		group.add(rb1);
		rb2=new JRadioButton("points");		
		group.add(rb2);
		rb3=new JRadioButton("dots");		
		group.add(rb3);
		rb4=new JRadioButton("pixels");	
		group.add(rb4);	
		rb5=new JRadioButton("crosses");	
		group.add(rb5);	
		rb6=new JRadioButton("error bars");	
		group.add(rb6);	
		rb7=new JRadioButton("circle");	
		group.add(rb7);	
		rb8=new JRadioButton("filled circle");	
		group.add(rb8);	
		rb9=new JRadioButton("square");	
		group.add(rb9);	
		rb10=new JRadioButton("filled square");	
		group.add(rb10);	
		rb11=new JRadioButton("diamond");	
		group.add(rb11);	
		rb12=new JRadioButton("f. diamond");	
		group.add(rb12);	
		rb13=new JRadioButton("triangle");	
		group.add(rb13);	
		rb14=new JRadioButton("f. triangle");	
		group.add(rb14);
		rb15=new JRadioButton("plus sign");	
		group.add(rb15);
		
		panel.add(marks);
		panel.add(rb1);
		panel.add(rb2);
		panel.add(rb3);
		panel.add(rb4);
		panel.add(rb5);
		panel.add(rb6);
		panel.add(rb7);
		panel.add(rb8);
		panel.add(rb9);
		panel.add(rb10);
		panel.add(rb11);
		panel.add(rb12);
		panel.add(rb13);
		panel.add(rb14);
		panel.add(rb15);
		
		JLabel color=new JLabel("Color:");
		colorButton=new JButton();
		colorButton.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				JColorChooser colorChooser=new JColorChooser();
				Color newColor=colorChooser.showDialog(BiochamMainFrame.frame, "DataSet Color", dsColor);
				dsColor=newColor;
				JButton b=(JButton)e.getSource();
				b.setBackground(newColor);
				b.setForeground(newColor);
				b.setName(newColor.toString());				
			}			
		});
		panel.add(color);
		panel.add(colorButton);
		
		impulses=new JCheckBox("Impulses");
		impulses.setSelected(false);
		impulses.addItemListener(new ItemListener(){

			public void itemStateChanged(ItemEvent e) {
				if(e.getStateChange()==ItemEvent.SELECTED){
					bars.setSelected(false);
				}				
			}});
		bars=new JCheckBox("Bars");
		bars.setSelected(false);
		bars.addItemListener(new ItemListener(){

			public void itemStateChanged(ItemEvent e) {
				if(e.getStateChange()==ItemEvent.SELECTED){
					impulses.setSelected(false);
				}				
			}});
		lines=new JCheckBox("Lines");
		lines.setSelected(false);
		panel.add(impulses);
		panel.add(bars);
		panel.add(lines);
		
		sl.putConstraint(SpringLayout.WEST, name, 5, SpringLayout.WEST, panel);
		sl.putConstraint(SpringLayout.WEST, nameText, 13, SpringLayout.EAST, name);
		sl.putConstraint(SpringLayout.NORTH, name, 20, SpringLayout.NORTH, panel);
		sl.putConstraint(SpringLayout.NORTH, nameText, 0, SpringLayout.NORTH, name);
		
		sl.putConstraint(SpringLayout.WEST, values, 5, SpringLayout.WEST, panel);
		sl.putConstraint(SpringLayout.WEST, tfValues, 9, SpringLayout.EAST, values);
		sl.putConstraint(SpringLayout.NORTH, values, 20, SpringLayout.SOUTH, name);
		sl.putConstraint(SpringLayout.NORTH, tfValues, 0, SpringLayout.NORTH, values);
		//addYValues
		sl.putConstraint(SpringLayout.WEST, addYValues, 5, SpringLayout.EAST, tfValues);
		sl.putConstraint(SpringLayout.NORTH, addYValues, 16, SpringLayout.SOUTH, nameText);
		sl.putConstraint(SpringLayout.WEST, b2, 2, SpringLayout.EAST, addYValues);
		sl.putConstraint(SpringLayout.NORTH, b2, 14, SpringLayout.SOUTH, nameText);
		
		sl.putConstraint(SpringLayout.WEST, marks, 5, SpringLayout.WEST, panel);
		
		sl.putConstraint(SpringLayout.WEST, rb1, 10, SpringLayout.EAST, marks);
		sl.putConstraint(SpringLayout.WEST, rb2, 7, SpringLayout.EAST, rb1);
		sl.putConstraint(SpringLayout.WEST, rb3, 7, SpringLayout.EAST, rb2);
		sl.putConstraint(SpringLayout.WEST, rb4, 7, SpringLayout.EAST, rb3);
		sl.putConstraint(SpringLayout.WEST, rb5, 7, SpringLayout.EAST, rb4);
		
		sl.putConstraint(SpringLayout.WEST, rb7, 10, SpringLayout.EAST, marks);
		sl.putConstraint(SpringLayout.WEST, rb8, 7, SpringLayout.EAST, rb7);
		sl.putConstraint(SpringLayout.WEST, rb9, 7, SpringLayout.EAST, rb8);
		sl.putConstraint(SpringLayout.WEST, rb10, 7, SpringLayout.EAST, rb9);
		
		sl.putConstraint(SpringLayout.WEST, rb11, 10, SpringLayout.EAST, marks);	
		sl.putConstraint(SpringLayout.WEST, rb12, 1, SpringLayout.EAST, rb11);
		sl.putConstraint(SpringLayout.WEST, rb13, 1, SpringLayout.EAST, rb12);
		sl.putConstraint(SpringLayout.WEST, rb14, 1, SpringLayout.EAST, rb13);
		
		sl.putConstraint(SpringLayout.WEST, rb15, 10, SpringLayout.EAST, marks);
		sl.putConstraint(SpringLayout.WEST, rb6, 5, SpringLayout.EAST, rb15);		
		
		sl.putConstraint(SpringLayout.NORTH, marks, 34, SpringLayout.SOUTH, values);
		sl.putConstraint(SpringLayout.NORTH, rb1, 0, SpringLayout.NORTH, marks);
		sl.putConstraint(SpringLayout.NORTH, rb2, 0, SpringLayout.NORTH, marks);
		sl.putConstraint(SpringLayout.NORTH, rb3, 0, SpringLayout.NORTH, marks);
		sl.putConstraint(SpringLayout.NORTH, rb4, 0, SpringLayout.NORTH, marks);		
		sl.putConstraint(SpringLayout.NORTH, rb5, 0, SpringLayout.NORTH, marks);
		
		sl.putConstraint(SpringLayout.NORTH, rb7, 5, SpringLayout.SOUTH, rb1);
		sl.putConstraint(SpringLayout.NORTH, rb8, 0, SpringLayout.NORTH, rb7);
		sl.putConstraint(SpringLayout.NORTH, rb9, 0, SpringLayout.NORTH, rb7);
		sl.putConstraint(SpringLayout.NORTH, rb10, 0, SpringLayout.NORTH, rb7);
		
		sl.putConstraint(SpringLayout.NORTH, rb11, 5, SpringLayout.SOUTH, rb7);
		sl.putConstraint(SpringLayout.NORTH, rb12, 0, SpringLayout.NORTH, rb11);
		sl.putConstraint(SpringLayout.NORTH, rb13, 0, SpringLayout.NORTH, rb11);
		sl.putConstraint(SpringLayout.NORTH, rb14, 0, SpringLayout.NORTH, rb11);
		
		sl.putConstraint(SpringLayout.NORTH, rb15,5, SpringLayout.SOUTH, rb11);
		sl.putConstraint(SpringLayout.NORTH, rb6, 0, SpringLayout.NORTH, rb15);
		
		sl.putConstraint(SpringLayout.WEST, color, 5, SpringLayout.WEST, panel);
		sl.putConstraint(SpringLayout.WEST, colorButton, 15, SpringLayout.EAST, color);
		sl.putConstraint(SpringLayout.NORTH, color, 15, SpringLayout.SOUTH, rb15);
		sl.putConstraint(SpringLayout.NORTH, colorButton, 3, SpringLayout.NORTH, color);
		
		sl.putConstraint(SpringLayout.WEST, impulses, 5, SpringLayout.WEST, panel);
		sl.putConstraint(SpringLayout.WEST, bars, 15, SpringLayout.EAST, impulses);
		sl.putConstraint(SpringLayout.WEST, lines, 15, SpringLayout.EAST, bars);
		sl.putConstraint(SpringLayout.NORTH, impulses, 10, SpringLayout.SOUTH, color);
		sl.putConstraint(SpringLayout.NORTH, bars, 0, SpringLayout.NORTH, impulses);
		sl.putConstraint(SpringLayout.NORTH, lines, 0, SpringLayout.NORTH, impulses);
		
	}
	
	public JPanel getNewDSPanel(){
		
		
		boolean b=SwingUtilities.isEventDispatchThread();
		
		nameText.setColumns(25);
		nameText.setEditable(true);
		tfValues.setColumns(25);
		tfValues.setText("");
		addYValues.setVisible(true);
		b2.setVisible(true);
		tfValues.setEditable(true);
		tfValues.setText(" Example: x1,y1;x2,y2; or x1,y1,z1;x2,y2,z2; ");
		nameText.setText("");
		colorButton.setBackground(null);
		colorButton.setForeground(null);
		colorButton.setName("");	
		rb6.setVisible(true);
		return panel;
	}
	
	public JPanel getDSPanel(){		
		
		return panel;
	}
	
	
	
	
	
	public JPanel getDSPanel(final DataSet ds){
		
		
		boolean b=SwingUtilities.isEventDispatchThread();
		
		nameText.setColumns(21);		
		tfValues.setColumns(21);
		addYValues.setVisible(false);
		tfValues.setEditable(false);
		nameText.setEditable(true);
		String n=ds.getLabelName();
		nameText.setText(n);
		nameText.setEditable(false);
		rb6.setVisible(false);
		b2.setVisible(false);
		SwingWorker sw=new SwingWorker(){
			
			String values="";
			@Override
			public Object construct() {
				
				for(int i=0;i<ds.getYValues().size();i++){
					values+=ds.getYValues().get(i);
					if(i<ds.getYValues().size()-1){
						values+=" , ";
					}
				}
				return null;
			}

			@Override
			public void finished() {
				tfValues.setText(values);
			}};
		sw.start();
		
		BiochamPlot p=model.getCurrentPlot();//.getPlot(0);
		int ind=p.getDatasetIndex(ds.getLabelName());		
		DataSet dss=p.getDataSetsList().get(ind);
		dsColor=dss.getColor();
		colorButton.setBackground(dsColor);
		colorButton.setForeground(dsColor);		
		impulses.setSelected(model.getCurrentPlot().getDrawingArea().getPlot().getImpulses());
		boolean barsSelected=model.getCurrentPlot().getDrawingArea().getPlot().getImpulses() || model.getCurrentPlot().getDrawingArea().getPlot().getConnected();
		bars.setSelected(!barsSelected);
		lines.setSelected(model.getCurrentPlot().getDrawingArea().getPlot().getConnected());
		
		return panel;
	}

	public String parseCSVFile(File f){ //*.csv or *.txt file with the certain format.
	
		BufferedReader br=null;
		InputStream is=null;
		String csvContents="";	
		//1,2,3;4,5,6;7,8,9; //or, 1,2;3,4;		
		boolean b=SwingUtilities.isEventDispatchThread();	
		String ext=f.getName().substring(f.getName().lastIndexOf(".")+1);
		
		
		if(ext.equals("xls")){
			try {
				
				is=new FileInputStream(f);
				HSSFWorkbook wb=new HSSFWorkbook(is);
				HSSFSheet sheet=wb.getSheetAt(0);				
				HSSFRow row=sheet.getRow(0);	
				int cellsNum=row.getLastCellNum();				
				int rows=sheet.getLastRowNum();
				boolean finish=false;
				for(int i=0;i<rows;i++){
					for(int j=0;j<cellsNum;j++){
						if(sheet.getRow(i).getCell(j)!=null){
							if(sheet.getRow(i).getCell(j).getCellType()==HSSFCell.CELL_TYPE_NUMERIC){
								csvContents+=sheet.getRow(i).getCell(j);
								if(j<cellsNum-1){
									csvContents+=",";
								}
							}else{
								finish=true;
								break;
							}	
						}else{
							finish=true;
							break;
						}						
					}if(finish){
						break;
					}else{
						csvContents+=";";
					}					
				}       				
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}else if(ext.equals("csv")){
			
			try {
				
				br = new BufferedReader(new FileReader(f));
				String line="";			
				int cnt=0;
				while((line=br.readLine())!=null){							
					StringTokenizer st=new StringTokenizer(line);				
					while(st.hasMoreTokens()){
							String as=st.nextToken();
							csvContents+=as+";";
					}      
					cnt++;
				}			
				br.close();	
				
				
				
			} catch (FileNotFoundException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
		
		
		
		
		return csvContents;		
	
	}
	
	
}

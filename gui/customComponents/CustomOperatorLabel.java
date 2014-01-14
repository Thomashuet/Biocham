package fr.inria.contraintes.biocham.customComponents;


import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.parsers.ParserCurveFit;
import fr.inria.contraintes.biocham.plotting.CurveFit;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.Color;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.io.File;
import java.util.ArrayList;

import javax.swing.JComponent;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextArea;
import javax.swing.SwingUtilities;




/**
 * Class thats creates custom labels that when clicked are performing some actions.
 * 
 * @author Dragana Jovanovska  
 */ 
public class CustomOperatorLabel extends CustomToolTipLabel{
	
	
	
	String aText="",name="";	
	BiochamModelElement element;
	ArrayList<String> query;
	JTextArea formula;
	String elementName="";
	String what;
	
	public CustomOperatorLabel(String n,String description,JTextArea fla){
		super(n,description);
		setName(n);
		MouseListener listener=new LabelMouseListener();
		addMouseListener(listener);
		formula=fla;
		elementName=n;
		setForeground(Color.BLUE);
		
	}
	public CustomOperatorLabel(String n,String description,JTextArea fla,String rule){
		super(n,description);
		setName(n);
		MouseListener listener=new LabelMouseListener();
		addMouseListener(listener);
		formula=fla;
		what=rule;
		setForeground(Color.BLUE);
	}
	
	
	
	public CustomOperatorLabel(String n,String description,BiochamModelElement me,JTextArea fla){
		
		super(n,description);
		setName(n);		
		element=me;
		MouseListener listener=new LabelMouseListener();
		addMouseListener(listener);	
		formula=fla;
		setForeground(Color.BLUE);
	}
	
	
	
	
	public CustomOperatorLabel(String el,String description, String n,String type,JTextArea fla){
		super(n,description);
		
		boolean b=SwingUtilities.isEventDispatchThread();
		setName(n);
		if(type.equals("molecule")){
			setForeground(Utils.moleculeOperatorColor);
		}else if(type.equals("parameter")){
			setForeground(Utils.parameterOperatorColor);
		}else{
			setForeground(Color.BLUE);
		}		 		
		MouseListener listener=new LabelMouseListener();
		addMouseListener(listener);
		formula=fla;
		elementName=el;
		name=el;
		what=type;
	}
	
	
	
	
	
	class LabelMouseListener extends MouseAdapter{
		
		public void mouseClicked(MouseEvent e){
			
			
			JComponent c=(JComponent)e.getSource();
			final JLabel l=(JLabel)c;		
			final String text=l.getText();
			setAText(text);
			if(text.contains("curve_fit")){
				int response=JOptionPane.showConfirmDialog(BiochamMainFrame.frame, "Do you want to load the parameters from an Excel file?");
				
				if(response==JOptionPane.OK_OPTION){
					
					
					Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
					String rep=Utils.showOpenDialog(BiochamMainFrame.frame,"excel");			
					if (rep!=null) {
						final File file=new File(rep);
						if(!file.isDirectory()){
				            SwingWorker sw=new SwingWorker(){
	
								@Override
								public Object construct() {
									parseCurveFitContent(file,text);
									return null;
								}
	
								@Override
								public void finished() {
									ArrayList<String> content=getQuery();
						            if(content!=null){
						            	 String s="";
								            for(int i=0;i<content.size();i++){
								            	s+=content.get(i);
								            	if (i<content.size()-1){
								            		s+=" & ";
								            	}
								            }
								            l.setName(s);  
								            formula.insert(s,formula.getCaretPosition());
						            }		
									
								}};
				            sw.start();			            
						}else{
							JOptionPane.showMessageDialog(BiochamMainFrame.frame,"Select a file, not a whole directory!", "Selection Warning",JOptionPane.WARNING_MESSAGE);
						}       
					}
				}else{
					l.setName(text);
					formula.insert(text,formula.getCaretPosition());
				}
			}else{
				if(elementName!=null){
					if(elementName.contains("LTL") || name.contains("Kinetics") || name.contains("Macro")){						
						formula.insert("["+text+"]",formula.getCaretPosition());
						Utils.debugMsg(elementName);
						if(!elementName.contains("Operators") && !elementName.contains("Kinetics")){
							formula.setCaretPosition(formula.getCaretPosition()-2);
						}else if(elementName.contains("Kinetics")){
							formula.setCaretPosition(formula.getCaretPosition());
						}
					}else{						
						formula.insert(text,formula.getCaretPosition());
						if(!elementName.contains("Operators")){
							if(what==null){
								if(text.endsWith(") ")){
									formula.setCaretPosition(formula.getCaretPosition()-2);	
								}else{
									formula.setCaretPosition(formula.getCaretPosition()-1);	
								}
							}							
						}else{
							if(what==null){
								formula.setCaretPosition(formula.getCaretPosition()+2);
							}
						}
					}
				}else{
				
					formula.insert(text,formula.getCaretPosition());
				}
			}
		
			
		}
		
		
		
		
		private ArrayList<CurveFit> parseCurveFitContent(File file,String version) {
			
			ArrayList<CurveFit> content=null;
			
			String filename=file.getName();
	        int index = filename.lastIndexOf('.');
			String extension = null;
			if (index>0 && index <= filename.length() - 2 ) {
        	   	extension=filename.substring(index+1);	          	 
			}	
			if(extension!=null){
				if(extension.equals("xls")){
					
					ParserCurveFit curveFitParser=new ParserCurveFit();
					
					if(version.contains("curve_fit_err")){
						content=curveFitParser.getCurveFitErrs(file);
					}else{
						//SWING WORKER PLS
						content=curveFitParser.getCurveFits(file);
						if(content==null){
							return null;
						}
					}
					setQuery(curveFitParser.getLtlQueries());
					ArrayList<String> sds;
					String cfVals="",cfVars="";
					for(int i=0;i< content.size();i++){
						sds=content.get(i).getSdsValues();
						for(int j=0;j<sds.size();j++){	
								if(i==content.size()-1 && j==sds.size()-1){
									cfVals+=sds.get(j);
								}else{
									cfVals+=sds.get(j)+",";
								}							
						}	
						sds=content.get(i).getSdsVariables();
						if(i<content.size()-1){
						
							cfVars+=sds.get(0)+",";
						}else{
					
							cfVars+=sds.get(0);
						}
						
					}
					
								
					element.setCfValues(cfVals);		
					element.setCfVariables(cfVars);
					
				}else{
					JOptionPane.showMessageDialog(BiochamMainFrame.frame,"Just valid Excel file is accepted.");
				}
			}
			
			return content;
		}
	}

	public String getAText() {
		return aText;
	}

	public void setAText(String text) {
		aText = text;
	}



	public ArrayList<String> getQuery() {
		return query;
	}



	public void setQuery(ArrayList<String> query) {
		this.query = query;
	}



	
}

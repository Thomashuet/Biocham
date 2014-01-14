package fr.inria.contraintes.biocham.dialogs;



import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.customComponents.GradientPanel;
import fr.inria.contraintes.biocham.customComponents.InfoToolTip;
import fr.inria.contraintes.biocham.customComponents.TextFieldLTL;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.SupportedSuffixes;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.BorderLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.JToolTip;
import javax.swing.SpringLayout;
import javax.swing.ToolTipManager;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.text.Document;

public class SatisfactionDegreeDialog extends JDialog implements ActionListener{

	
	
	BiochamModel model;
	Container c;
	GradientPanel contents;
	SpringLayout layout;
	TextFieldLTL qfltlText;
	JTextField variables,varValues,simTime;
	BiochamModelElement modelElement;
	String command;
	JFrame parentFrame;
	
	
	public SatisfactionDegreeDialog(JFrame parent,BiochamModelElement element){
		  super (parent, true);
		  parentFrame=parent;
		  model=element.getModel();
		  modelElement=element;
		  initComponents("Satisfaction Degree Computation");
		  pack();
	}

		
	private void initComponents(String title) {
		
		setTitle(title);
	    c = getContentPane();
	    contents=new GradientPanel();
	    layout= new SpringLayout();
	    contents.setLayout(layout);
		c.setLayout(new BorderLayout());
		
		JLabel qfltl=new JLabel("QFLTL Query: ");
		if(model.simulationMap.get("cmaesQuery")!=null){
			qfltlText=new TextFieldLTL(parentFrame,"QFLTL",model.simulationMap.get("cmaesQuery"),modelElement);
		}else{
			if(model.simulationMap.get("ltlQuery")!=null){
				qfltlText=new TextFieldLTL(parentFrame,"QFLTL",model.simulationMap.get("ltlQuery"),modelElement);
			}else{
				qfltlText=new TextFieldLTL(parentFrame,"QFLTL","double click to compose",modelElement);
			}
		}
		qfltlText.setColumns(33);
		variables=new JTextField(33);
		varValues=new JTextField(33);
		simTime=new JTextField(33);
		
		Document document =	qfltlText.getDocument();
		document.addDocumentListener( new DocumentListener(){

			public void changedUpdate(DocumentEvent e) {
				if(qfltlText.getText().contains("curve_fit")){
					variables.setText(modelElement.getCfVariables());
					varValues.setText(modelElement.getCfValues());
					modelElement.cfVariables="";
					modelElement.cfValues="";
				}
				
				
			}

			public void insertUpdate(DocumentEvent e) {
				if(qfltlText.getText().contains("curve_fit")){
					if(modelElement!=null){
						if(modelElement.getCfVariables()!=null){
							variables.setText(modelElement.getCfVariables());
							varValues.setText(modelElement.getCfValues());
							modelElement.cfVariables="";
							modelElement.cfValues="";
						}
					}
				}
			}

			public void removeUpdate(DocumentEvent e) {
				if(qfltlText.getText().contains("curve_fit")){
					variables.setText(modelElement.getCfVariables());
					varValues.setText(modelElement.getCfValues());
					modelElement.cfVariables="";
					modelElement.cfValues="";
				}
			}} );
		
		String txt=null;
		txt=model.simulationMap.get("landscapeQuery");		
		if(txt!=null){
			qfltlText.setText(txt);
		}
		
		JButton loadVar=new JButton("Load");
		JButton loadQuery=new JButton("Load");	
		loadQuery.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				
				final Object c=e.getSource();
				Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
				String rep=Utils.showOpenDialog(BiochamMainFrame.frame,SupportedSuffixes.TEXT_SUFFIX);			
				if (rep!=null) {
					final File file=new File(rep);
		            if(!file.isDirectory()){
			            SwingWorker sw=new SwingWorker(){
			            	String content;
							@Override
							public Object construct() {
								content=Utils.getFileContent(file);	
								return content;
							}
	
							@Override
							public void finished() {
	 	           	            
								qfltlText.setText(content);
								model.simulationMap.put("landscapeQuery",content);					    								
							}
						};
						sw.start();		
		            }else{
						JOptionPane.showMessageDialog(BiochamMainFrame.frame,"Select a file, not a whole directory!", "Selection Warning",JOptionPane.WARNING_MESSAGE);
					}
				}
			}
		});
		JLabel b2=new JLabel(Icons.icons.get("infoAssist.png"+0.7)){
			public JToolTip createToolTip() {
				return new InfoToolTip("projectImages/exampleLoadingQuery.png");
			}
		};    	 
		b2.setToolTipText("");	    	 
		b2.addMouseListener(new MouseAdapter(){
			public void mouseClicked(MouseEvent e) {
				if(e.getSource() instanceof JLabel){
   	    			ToolTipManager.sharedInstance().setInitialDelay(0);    	    		
   	    			ToolTipManager.sharedInstance().setEnabled(true);   	   
   	    		}
   	    	}	    	    	
   	    });
		contents.add(qfltl);
	    contents.add(qfltlText);
	    contents.add(loadQuery);
	    contents.add(b2);
	    
	    layout.putConstraint(SpringLayout.WEST, qfltl, 15, SpringLayout.WEST, contents);
  	    layout.putConstraint(SpringLayout.NORTH, qfltl, 40,SpringLayout.NORTH, contents);
  	    layout.putConstraint(SpringLayout.NORTH, qfltlText, 0,SpringLayout.NORTH, qfltl);
	    layout.putConstraint(SpringLayout.WEST, qfltlText, 25,SpringLayout.EAST, qfltl);	    
	    layout.putConstraint(SpringLayout.WEST, loadQuery, 10,SpringLayout.EAST, qfltlText);
	    layout.putConstraint(SpringLayout.NORTH, loadQuery, 36,SpringLayout.NORTH, contents);
	    layout.putConstraint(SpringLayout.WEST, b2, 2,SpringLayout.EAST, loadQuery);
	    layout.putConstraint(SpringLayout.NORTH, b2, 0,SpringLayout.NORTH, qfltl);
		
	    
	    JLabel var=new JLabel("<html><u>Variables:</u>");
	    txt=model.simulationMap.get("landscapeVars");
	    if(txt!=null){
	    	variables.setText(txt);
	    }
		loadVar.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				
				final Object c=e.getSource();
				Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
				String rep=Utils.showOpenDialog(BiochamMainFrame.frame,SupportedSuffixes.TEXT_SUFFIX);			
				if (rep!=null) {
					final File file=new File(rep);
		            if(!file.isDirectory()){
			            SwingWorker sw=new SwingWorker(){
			            	String content;
							@Override
							public Object construct() {
								content=Utils.getFileContent(file);	
								return content;
							}
	
							@Override
							public void finished() {
	 	           	            
					            variables.setText(content);
					            String t=((JButton)c).getName();
					            model.simulationMap.put("landscapeVars",content);
					    								
							}
						};
						sw.start();
		            }else{
						JOptionPane.showMessageDialog(BiochamMainFrame.frame,"Select a file, not a whole directory!", "Selection Warning",JOptionPane.WARNING_MESSAGE);
					}
		            
				}
				
			}});
		JLabel b3=new JLabel(Icons.icons.get("infoAssist.png"+0.7)){
			public JToolTip createToolTip() {
				return new InfoToolTip("projectImages/exampleLoadingVariables.png");
			}
		};    	 
		b3.setToolTipText("");	    	 
		b3.addMouseListener(new MouseAdapter(){
			public void mouseClicked(MouseEvent e) {
				if(e.getSource() instanceof JLabel){
   	    			ToolTipManager.sharedInstance().setInitialDelay(0);    	    		
   	    			ToolTipManager.sharedInstance().setEnabled(true);   	   
   	    		}
   	    	}	    	    	
   	    }); 
		
		contents.add(var);
		contents.add(variables);
		contents.add(loadVar);
		contents.add(b3);
		
		layout.putConstraint(SpringLayout.WEST, var, 15, SpringLayout.WEST, contents);
	    layout.putConstraint(SpringLayout.NORTH, var, 20,SpringLayout.SOUTH, qfltlText);  	    
	    layout.putConstraint(SpringLayout.WEST, variables, 0,SpringLayout.WEST,qfltlText);
	    layout.putConstraint(SpringLayout.NORTH, variables, 20,SpringLayout.SOUTH, qfltlText);    
	    layout.putConstraint(SpringLayout.WEST, loadVar, 10,SpringLayout.EAST, variables);
	    layout.putConstraint(SpringLayout.NORTH, loadVar,17,SpringLayout.SOUTH, qfltlText);
	    layout.putConstraint(SpringLayout.WEST, b3, 2,SpringLayout.EAST, loadVar);
	    layout.putConstraint(SpringLayout.NORTH, b3, 20,SpringLayout.SOUTH, qfltlText);
	    
	    
	    
	    JLabel simTimeL=new JLabel("<html><u>Simulation Time:</u>");
	    txt=model.simulationMap.get("numSimDuration");	    
	    if(txt!=null){
	    	simTime.setText(txt);
	    }
	    
	    JLabel val=new JLabel("<html><u>Values:</u>");
	    txt=model.simulationMap.get("landscapeVals");
	    
	    if(txt!=null){
	    	varValues.setText(txt);
	    }
		JButton loadVal=new JButton("Load");
		loadVal.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				
				final Object c=e.getSource();
				Utils.fileChooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
				String rep=Utils.showOpenDialog(BiochamMainFrame.frame,SupportedSuffixes.TEXT_SUFFIX);			
				if (rep!=null) {
		               final File file = new File(rep);
		               if(!file.isDirectory()){
			            	   SwingWorker sw=new SwingWorker(){
	
				            	String content;
								@Override
								public Object construct() {
									 content=Utils.getFileContent(file);
									return content;
								}
		
								@Override
								public void finished() {
									varValues.setText(content);									
									model.simulationMap.put("landscapeVals",content);
						    			
								}
				            };
				            sw.start();                    
		               }else{
							JOptionPane.showMessageDialog(BiochamMainFrame.frame,"Select a file, not a whole directory!", "Selection Warning",JOptionPane.WARNING_MESSAGE);
						}
		            
		            
				}
				
			}});
		
		JLabel b4=new JLabel(Icons.icons.get("infoAssist.png"+0.7)){
			public JToolTip createToolTip() {
				return new InfoToolTip("projectImages/exampleLoadingValues.png");
			}
		};    	 
		b4.setToolTipText("");	    	 
		b4.addMouseListener(new MouseAdapter(){
			public void mouseClicked(MouseEvent e) {
				if(e.getSource() instanceof JLabel){
   	    			ToolTipManager.sharedInstance().setInitialDelay(0);    	    		
   	    			ToolTipManager.sharedInstance().setEnabled(true);   	   
   	    		}
   	    	}	    	    	
   	    }); 
		contents.add(val);
	    contents.add(varValues);
	    contents.add(loadVal);
	    contents.add(b4);
	    
	    layout.putConstraint(SpringLayout.WEST, val, 15, SpringLayout.WEST, contents);
	    layout.putConstraint(SpringLayout.NORTH, val, 20,SpringLayout.SOUTH, var);  	    
	    layout.putConstraint(SpringLayout.WEST, varValues, 0, SpringLayout.WEST, qfltlText);
	    layout.putConstraint(SpringLayout.NORTH, varValues, 0,SpringLayout.NORTH, val);    
	    layout.putConstraint(SpringLayout.WEST, loadVal, 10,SpringLayout.EAST, varValues);
	    layout.putConstraint(SpringLayout.NORTH, loadVal, 21,SpringLayout.SOUTH, var);
	    layout.putConstraint(SpringLayout.WEST, b4, 2,SpringLayout.EAST, loadVal);
	    layout.putConstraint(SpringLayout.NORTH, b4,20,SpringLayout.SOUTH, var);
	    
	    contents.add(simTimeL);
	    contents.add(simTime);	   
	    
	    layout.putConstraint(SpringLayout.WEST, simTimeL, 15, SpringLayout.WEST, contents);
	    layout.putConstraint(SpringLayout.NORTH, simTimeL, 20,SpringLayout.SOUTH, val);  	    
	    layout.putConstraint(SpringLayout.WEST, simTime, 0, SpringLayout.WEST, qfltlText);
	    layout.putConstraint(SpringLayout.NORTH, simTime, 0,SpringLayout.NORTH, simTimeL);
	  
	    
	    
	    JButton compute=new JButton("Compute");
	    compute.setActionCommand("computeSatisfactionDegree");
	    compute.addActionListener(this);
	    JButton cancel=new JButton("Cancel");
	    cancel.setActionCommand("cancel");
	    cancel.addActionListener(this);
	   
	    contents.add(compute);
 	    contents.add(cancel);
 	    
 	    layout.putConstraint(SpringLayout.WEST, compute, 200, SpringLayout.WEST, contents);
	    layout.putConstraint(SpringLayout.NORTH, compute, 40,SpringLayout.SOUTH, simTime);  
	    layout.putConstraint(SpringLayout.WEST, cancel, 10, SpringLayout.EAST, compute);
	    layout.putConstraint(SpringLayout.NORTH, cancel,40,SpringLayout.SOUTH, simTime);  
 	    
	   
	    c.add(contents,BorderLayout.CENTER);
	    Point pos = BiochamMainFrame.frame.getLocationOnScreen();
	    setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
	    setResizable(true);
	    if(Utils.is_OS_MAC){
	    	setSize(new Dimension(650,300));
	    }else{
	    	setSize(new Dimension(600,300));
	    }   
	    setLocationRelativeTo(BiochamMainFrame.frame);
	    setVisible(true);		
	}

	public String getCommand(){
		return command;
	}
	
	
	public void actionPerformed(ActionEvent e) {
		if(e.getActionCommand().equals("cancel")){
			setVisible(false);
			dispose();
		}else{
			
			StringBuffer buf= new StringBuffer("satisfaction_degree(");			  
			if(qfltlText.getText().length()>0 && qfltlText.getText()!=""){
			
				buf.append(qfltlText.getText()+",");
				model.simulationMap.put("landscapeQuery",qfltlText.getText());
				if(variables.getText().length()>0 && variables.getText()!=""){
					model.simulationMap.put("landscapeVars", variables.getText());
					buf.append("["+variables.getText()+"],");
			
					if(varValues.getText().length()>0 && varValues.getText()!=""){
						if(simTime.getText()!=null && simTime.getText()!="" && simTime.getText()!="0"){
							buf.append("["+varValues.getText()+"]"+","+simTime.getText().trim()+").\n");
						}else{
							buf.append("["+varValues.getText()+"]"+",20).\n");	
						}
						
						model.simulationMap.put("landscapeVals", varValues.getText());
						command = buf.toString();
					}else{
						command = null;
					}
				}else{
					command = null;
				}
			}else{
				command = null;
		 	}
			buf=null;
			setVisible(false);
			dispose();
		}
	}
}

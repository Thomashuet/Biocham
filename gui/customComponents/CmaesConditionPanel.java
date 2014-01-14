package fr.inria.contraintes.biocham.customComponents;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.dialogs.DialogOptions;
import fr.inria.contraintes.biocham.dialogs.DialogSearchParams;
import fr.inria.contraintes.biocham.dialogs.InputDialogWithCombobox;
import fr.inria.contraintes.biocham.graphicalEditor.CustomIconedCellRenderer;
import fr.inria.contraintes.biocham.plotting.CmaesCondition;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.SupportedSuffixes;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;
import net.java.balloontip.BalloonTip;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JComboBox;
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
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.text.Document;



/**
 * Class thats creates the panel for searching model's parameters using the CMAES algorithm. This panel is used by the DialogSearchParams dialog for searching model parameters.
 * 
 * @author Dragana Jovanovska  
 */ 
public class CmaesConditionPanel extends GradientPanel implements ActionListener{

	
	JTextField variables,varValues,qfltlText,pvaluesText,chosenParams;
	BiochamModelElement me;
	CustomMultiSelectComboBox p1;
	String[] list;
	String result;
	DialogSearchParams parent;
	JFrame parentFrame;
	
	public CmaesConditionPanel(DialogSearchParams p, int conditions,BiochamModelElement modelElement, String[] l){
		super();
		parentFrame=p.getParentFrame();
		list=l;
		parent=p;
		ArrayList<String> parameters=new ArrayList<String>();
		me=modelElement;
		SpringLayout sl=new SpringLayout();
		setLayout(sl);
		JLabel multiConditions =new JLabel("<html><u> "+conditions+". Condition: </u></html>");
		add(multiConditions);
		
		JLabel qfltl=new JLabel("QFLTL Query: ");
		
		if(me.getModel().simulationMap.get("cmaesQuery")!=null){
			qfltlText=new TextFieldLTL(parentFrame,"QFLTL",me.getModel().simulationMap.get("cmaesQuery"),me);
		}else{
			if(me.getModel().simulationMap.get("ltlQuery")!=null){
				qfltlText=new TextFieldLTL(parentFrame,"QFLTL",me.getModel().simulationMap.get("ltlQuery"),me);
			}else{
				qfltlText=new TextFieldLTL(parentFrame,"QFLTL","double click to compose",me);
			}
		}
		qfltlText.setColumns(33);
		variables=new JTextField(33);
		varValues=new JTextField(33);		 
		Document document =	qfltlText.getDocument();		
		document.addDocumentListener( new DocumentListener(){
			
			public void changedUpdate(DocumentEvent e) {	
				
				
				if(qfltlText.getText().contains("curve_fit")){
					variables.setText(me.getCfVariables());
					varValues.setText(me.getCfValues());
					me.cfVariables="";
					me.cfValues="";
				}				
			}
			public void insertUpdate(DocumentEvent e) {
				if(qfltlText.getText().contains("curve_fit")){
					if(me!=null){
						if(me.getCfVariables()!=null){
							variables.setText(me.getCfVariables());
							varValues.setText(me.getCfValues());
							me.cfVariables="";
							me.cfValues="";
						}
					}
				}
			}
			public void removeUpdate(DocumentEvent e) {
				if(qfltlText.getText().contains("curve_fit")){
					variables.setText(me.getCfVariables());
					varValues.setText(me.getCfValues());
					me.cfVariables="";
					me.cfValues="";
				}
			}} );		
		String txt=me.getModel().simulationMap.get("cmaesQuery");
		if(txt==null){
				txt=me.getModel().simulationMap.get("ltlQuery");
		}
		if(txt!=null){
			qfltlText.setText(txt);
		}
		
		JButton loadVar=new JButton("Load");
		JButton loadQuery=new JButton("Load");//load or write
		loadQuery.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				
				
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
								me.getModel().simulationMap.put("cmaesQuery",content);				    						
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
		
		add(qfltl);
		add(qfltlText);
		add(loadQuery);
		add(b2);		
	    
		sl.putConstraint(SpringLayout.WEST, multiConditions, 200, SpringLayout.WEST, this);
		sl.putConstraint(SpringLayout.NORTH, multiConditions, 20,SpringLayout.NORTH, this);
		sl.putConstraint(SpringLayout.WEST, qfltl, 15, SpringLayout.WEST, this);
		sl.putConstraint(SpringLayout.NORTH, qfltl, 70,SpringLayout.NORTH, this);
	    
		sl.putConstraint(SpringLayout.NORTH, qfltlText, 0,SpringLayout.NORTH, qfltl);
		sl.putConstraint(SpringLayout.WEST, qfltlText, 5,SpringLayout.EAST, qfltl);
	    
		sl.putConstraint(SpringLayout.WEST, loadQuery, 0,SpringLayout.WEST, loadVar);
		sl.putConstraint(SpringLayout.NORTH, loadQuery, 65,SpringLayout.NORTH, this);
	    
		sl.putConstraint(SpringLayout.WEST, b2, 2,SpringLayout.EAST, loadQuery);
		sl.putConstraint(SpringLayout.NORTH, b2, 60,SpringLayout.NORTH, this);
		    
	   
	 
	    JLabel param = new JLabel("<html><u>Parameters:</u>");
	    add(param);
	    sl.putConstraint(SpringLayout.WEST, param, 15,SpringLayout.WEST, this);   
	    sl.putConstraint(SpringLayout.NORTH, param, 40,SpringLayout.SOUTH, qfltlText);
	     
	    
	    for(int i=1;i<list.length;i++){
	    	parameters.add(list[i]);
	    }
	    
	    p1 = (new CustomComboBox(list,false)).getMultiSelectComboBox();    
	    Dimension d = p1.getPreferredSize();
	    p1.setPreferredSize(new Dimension(130, d.height));
	    p1.setPopupWidth(d.width+20);	  
	    p1.setName(Integer.toString(4));	    
	    final BalloonTip bTip=new BalloonTip(p1,"Don't forget to press ENTER to apply the selection.",Utils.modern,BalloonTip.Orientation.RIGHT_ABOVE,BalloonTip.AttachLocation.NORTHEAST,20,10,false);
		bTip.setText("Don't forget to press ENTER to apply the selection.");
		bTip.setIcon(Icons.icons.get("flag_blue.png"));
		bTip.setIconTextGap(10);	
		bTip.setVisible(false);
		bTip.enableClickToHide(true);		
		p1.addPopupMenuListener(new PopupMenuListener(){
			public void popupMenuCanceled(PopupMenuEvent e) {
					int k=p1.getParentClass().getSelected().size();
					if(k>0){
						bTip.setVisible(true);
					}else{
						bTip.setVisible(false);
					}
			}
			public void popupMenuWillBecomeInvisible(PopupMenuEvent e) {
							
			}
			public void popupMenuWillBecomeVisible(PopupMenuEvent e) {
				bTip.setVisible(false);
				
			}});	
	    p1.addKeyListener(new KeyAdapter(){
	    	
	    	public void keyPressed(KeyEvent e){
        		if(e.getSource() instanceof CustomMultiSelectComboBox){
        			if(e.getKeyCode()==KeyEvent.VK_ENTER){        				
        				
        				CustomMultiSelectComboBox mcb=(CustomMultiSelectComboBox)e.getSource();
        				ArrayList selected=mcb.getParentClass().getSelected();
        				
        				if(selected.size()>0){
        					
        					boolean allSelected=false;
            				bTip.setVisible(false);        				
            				mcb.getParentClass().setFinished(true);
            				mcb.setPopupVisible(false);
        					ArrayList all = null;
            				for(int i=0;i<selected.size();i++){
            					if(selected.get(i).equals("Select All")){
            						allSelected=true;
            						all=mcb.getParentClass().getAllItems();
            					}
            				}
            				if(allSelected){        					
            					selected=all;
            				}
            				String atext=pvaluesText.getText();
            				String items=chosenParams.getText();
            				int k=selected.size();        				       
            		       
            		  	   String newValue=null;
            		  	   JComboBox cbox=new JComboBox(list);
            		  	   cbox.setEditable(false);
            		  	   InputDialogWithCombobox input=new InputDialogWithCombobox(BiochamMainFrame.frame,"Choose parameter for the assignment:\n","Parameter Change",cbox,mcb.getParentClass());  		  	
            		  	  
            		  	   newValue=input.getChosenValue();
            		  	   
            		  	   if(newValue!=null){
            		  		   
            		  		   pvaluesText.setEditable(true);	
            		  		   if(atext!=null && atext!=""){
            		  			   if(atext.length()>0){
            		  				   for(int i=0;i<k;i++){
            		  					   atext+=","+newValue+"";
            		  				   }
            		  				   pvaluesText.setText(atext);
            		  			   }else{
            		  				   atext=""+newValue+"";
            		  				   for(int i=1;i<k;i++){
            		  					   atext+=","+newValue+"";
            		  				   }
            		  				   pvaluesText.setText(atext);
            		  			   }
            		  		   }
            		  		   chosenParams.setEditable(true);
            		  		   if(items!=null && items!=""){
            		  			   if(items.length()>0){
            		  				   for(int i=0;i<selected.size();i++){
            		  					   items+=","+selected.get(i);        		        		
            		  				   }
            		  				   chosenParams.setText(items);
            		  			   }else{
            		  				   items+=selected.get(0);
            		  				   for(int i=1;i<selected.size();i++){
            		  					   items+=","+selected.get(i);     
            		  				   }  	        		        	
            		  				   chosenParams.setText(items);
            		  			   }
            		  		   }
            		  		   chosenParams.setEditable(false);        		         		        
            		  		   CustomIconedCellRenderer rdr=mcb.getParentClass().getRenderer();    		      
            		  		   Vector selectedObjectsVector=rdr.getSelectedObjects();
            		  		   for(int i=0;i<selectedObjectsVector.size();i++){
            		  			   mcb.removeItem(selectedObjectsVector.get(i));
            		  			   mcb.getParentClass().getAllItems().remove(selectedObjectsVector.get(i));
            		  		   }     
            		  		   pvaluesText.setEditable(false);		 
            		  		   selected.clear();        		  	  
            		  		   int rest=mcb.getItemCount();
            		  		   if(rest==1 && mcb.getItemAt(0).equals("Select All")){
            		  			   mcb.setEnabled(false);        		  		  
            		  		   }
            		  		   rdr.unselectAll();        		  	
            		  		   mcb.getParentClass().setFinished(false);
            		  	   }else{
            		  		   CustomIconedCellRenderer rdr=mcb.getParentClass().getRenderer();	        
            		  		   int rest=mcb.getItemCount();
            		  		   rdr.unselectAll();        		  	
            		  		   mcb.getParentClass().setFinished(false);
            		  		   mcb.getParentClass().desellectAll();
            		  		   if(rest==1 && mcb.getItemAt(0).equals("Select All")){
            		  			   mcb.setEnabled(false);        		  		  
            		  		   }      		  		   
            		  	   }
        				}else{
        					 CustomIconedCellRenderer rdr=mcb.getParentClass().getRenderer();	        
        					 int rest=mcb.getItemCount();
        					 rdr.unselectAll();  
        					 mcb.getParentClass().desellectAll();
        					 mcb.getParentClass().setFinished(false);
        					 if(rest==1 && mcb.getItemAt(0).equals("Select All")){
        						 mcb.setEnabled(false);        		  		  
        					 }
        					
        				}
        				
        			}
        			bTip.setVisible(false);
        		}
	    	}
	    });
	    
	    JLabel pvalues = new JLabel("<html><u>Values:</u>");
	    pvaluesText=new JTextField();
	    pvaluesText.setColumns(23);
	    pvaluesText.setEditable(false);
	    chosenParams=new JTextField(49);
	    chosenParams.setEditable(false);
	   
	    add(p1);
	    add(pvalues);
	    add(pvaluesText);
	    add(chosenParams);
	    
	   
	    sl.putConstraint(SpringLayout.WEST, p1, 15,SpringLayout.EAST, param);
	    sl.putConstraint(SpringLayout.NORTH, p1, 35,SpringLayout.SOUTH, qfltlText);	    
	    sl.putConstraint(SpringLayout.WEST, pvalues, 15,SpringLayout.EAST, p1);
	    sl.putConstraint(SpringLayout.NORTH, pvalues, 0,SpringLayout.NORTH, param);	   
	    sl.putConstraint(SpringLayout.WEST, pvaluesText, 10,SpringLayout.EAST, pvalues);
	    sl.putConstraint(SpringLayout.NORTH, pvaluesText,37,SpringLayout.SOUTH, qfltlText);  
	    sl.putConstraint(SpringLayout.WEST, chosenParams, 0,SpringLayout.WEST, param);
	    sl.putConstraint(SpringLayout.NORTH, chosenParams,10,SpringLayout.SOUTH, param);
	  	  	    
	    JLabel var=new JLabel("<html><u>Variables:</u>");
	    
	    txt=me.getModel().simulationMap.get("cmaesVars");	    	    
	    if(txt!=null){
	    	variables.setText(txt);
	    }
		loadVar.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				
				
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
					            //String t=((JButton)c).getName();
					            me.getModel().simulationMap.put("cmaesVars", content);				    							
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
		
		add(var);
		add(variables);
		add(loadVar);
		add(b3);
		
		sl.putConstraint(SpringLayout.WEST, var, 0, SpringLayout.WEST, chosenParams);
		sl.putConstraint(SpringLayout.NORTH, var, 50,SpringLayout.SOUTH, chosenParams);  	    
		sl.putConstraint(SpringLayout.WEST, variables, 0,SpringLayout.WEST, p1);
		sl.putConstraint(SpringLayout.NORTH, variables, 50,SpringLayout.SOUTH, chosenParams);    
		sl.putConstraint(SpringLayout.WEST, loadVar, 15,SpringLayout.EAST, variables);
		sl.putConstraint(SpringLayout.NORTH, loadVar, 45,SpringLayout.SOUTH, chosenParams);
		sl.putConstraint(SpringLayout.WEST, b3, 2,SpringLayout.EAST, loadVar);
		sl.putConstraint(SpringLayout.NORTH, b3, 35,SpringLayout.SOUTH, chosenParams);
	    
	    JLabel val=new JLabel("<html><u>Values:</u>");
	   
	    txt=me.getModel().simulationMap.get("cmaesVals");	    
	    
	    if(txt!=null){
	    	varValues.setText(txt);
	    }
		JButton loadVal=new JButton("Load");
		loadVal.addActionListener(new ActionListener(){

			public void actionPerformed(ActionEvent e) {
				
				
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
								varValues.setText(content);
								me.getModel().simulationMap.put("cmaesVals", content);				    		
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
		
		add(val);
		add(varValues);
		add(loadVal);
		add(b4);
		
		
		sl.putConstraint(SpringLayout.WEST, val, 0, SpringLayout.WEST, var);
		sl.putConstraint(SpringLayout.NORTH, val, 20,SpringLayout.SOUTH, var);  	    
		sl.putConstraint(SpringLayout.WEST, varValues, 0,SpringLayout.WEST, p1);
		sl.putConstraint(SpringLayout.NORTH, varValues, 0,SpringLayout.NORTH, val);    
		sl.putConstraint(SpringLayout.WEST, loadVal, 15,SpringLayout.EAST, varValues);
	    sl.putConstraint(SpringLayout.NORTH, loadVal, 15,SpringLayout.SOUTH, var);
	    sl.putConstraint(SpringLayout.WEST, b4, 2,SpringLayout.EAST, loadVal);
	    sl.putConstraint(SpringLayout.NORTH, b4, 5,SpringLayout.SOUTH, var);
	    
	     
	   
	    
	    JButton search=new JButton("Search");
	    search.setActionCommand("multiCondCmaesSearch");
	    search.addActionListener(this);
	    
	    JButton nextCondition=new JButton("More Conditions");
	    nextCondition.setActionCommand("addAnotherSearchCondition");
	    nextCondition.addActionListener(this);
	    
	    JButton cancel=new JButton("Cancel");
	    cancel.setActionCommand("cancel");
	    cancel.addActionListener(this);
	    add(search);
	    add(nextCondition);
	    add(cancel);
	    
	    sl.putConstraint(SpringLayout.WEST, search, 160, SpringLayout.WEST, this);
	   	sl.putConstraint(SpringLayout.NORTH, search, 70,SpringLayout.SOUTH, varValues); 
	   	sl.putConstraint(SpringLayout.WEST, nextCondition, 10, SpringLayout.EAST, search);
	   	sl.putConstraint(SpringLayout.NORTH, nextCondition, 70,SpringLayout.SOUTH, varValues); 
	   	sl.putConstraint(SpringLayout.WEST, cancel, 10, SpringLayout.EAST, nextCondition);
	   	sl.putConstraint(SpringLayout.NORTH, cancel, 70,SpringLayout.SOUTH, varValues); 
	}


	public void actionPerformed(ActionEvent e) {
		
		String cmd=e.getActionCommand();
		setResult(cmd);
		if(cmd.equals("cancel")){
			DialogSearchParams.cmaesConditions.clear();
			parent.dispose();
			result=null;
			parent.setResult(result);
		}else{
			
			//String query,String lVars,String lVals, String lModif
			//variables,varValues,qfltlText,pvaluesText,chosenParams;
			
			String modifPairs="";
			StringTokenizer st1=new StringTokenizer(chosenParams.getText(),",");
			StringTokenizer st2=new StringTokenizer(pvaluesText.getText(),",");
			int nbr=st1.countTokens();
			for(int i=0;i<nbr;i++){
				modifPairs+="("+st1.nextToken()+","+st2.nextToken()+")";
				if(i<nbr-1){
					modifPairs+=",";
				}
			}			
			CmaesCondition cond=new CmaesCondition(qfltlText.getText(),"["+variables.getText()+"]","["+varValues.getText()+"]","["+modifPairs+"]");
			DialogSearchParams.cmaesConditions.add(cond);
			
			if(cmd.equals("multiCondCmaesSearch")){
				
				((DialogSearchParams)parent).startSearch();
				
			}else if(cmd.equals("addAnotherSearchCondition")){
				((DialogSearchParams)parent).addNewConditionPanel();
			}
		} 
			
		//cancel
		//multiCondCmaesSearch
		//addAnotherSearchCondition
	}


	public String getResult() {
		return result;
	}


	public void setResult(String result) {
		this.result = result;
	}
	
}

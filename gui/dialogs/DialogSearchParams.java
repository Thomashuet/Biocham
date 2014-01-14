package fr.inria.contraintes.biocham.dialogs;

import net.java.balloontip.BalloonTip;
import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.customComponents.CmaesConditionPanel;
import fr.inria.contraintes.biocham.customComponents.CustomComboBox;
import fr.inria.contraintes.biocham.customComponents.CustomMultiSelectComboBox;
import fr.inria.contraintes.biocham.customComponents.GradientPanel;
import fr.inria.contraintes.biocham.customComponents.InfoToolTip;
import fr.inria.contraintes.biocham.customComponents.TextFieldLTL;
import fr.inria.contraintes.biocham.graphicalEditor.CustomIconedCellRenderer;
import fr.inria.contraintes.biocham.modelData.ParamTableLTLSpecifications;
import fr.inria.contraintes.biocham.modelData.ParamTableParameters;
import fr.inria.contraintes.biocham.modelData.ParamTableParameters.Parameter;
import fr.inria.contraintes.biocham.plotting.CmaesCondition;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.SupportedSuffixes;
import fr.inria.contraintes.biocham.utils.SwingWorker;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.BorderLayout;
import java.awt.CardLayout;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Point;
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

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.JToolTip;
import javax.swing.Spring;
import javax.swing.SpringLayout;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import javax.swing.event.PopupMenuEvent;
import javax.swing.event.PopupMenuListener;
import javax.swing.text.Document;



public class DialogSearchParams extends JDialog implements ActionListener{
	

	private BiochamModelElement modelElement;	
	BiochamModel model;
    Spring maxSpring;
    String dialogName;
    Container contents;
    SpringLayout layout;
    JPanel panel;
    static float lastSimLength;
    private String CMD_CANCEL = "cmd.cancel"/*NOI18N*/;
    private String CMD_OK = "cmd.ok"/*NOI18N*/;
    String result = null,bcomand=null;
    String[] list;
    CustomMultiSelectComboBox p1;
    CustomMultiSelectComboBox p2;
    CustomMultiSelectComboBox p3;
	CustomIconedCellRenderer renderer;
    JTextField t12, t13, t22, t23, t32, t33;
    Vector<Parameter> params;
    JLabel ltlLabel;
    JTextField ltlText,timeText,stepsText,qfltlText,pvaluesText,chosenParams,variables,varValues,
    		   iteration,objFitness,samplesText,variation,seedTf;
    ArrayList<String> parameters;
    boolean with_log=false;
    static int code;
    boolean clickedEnter=false;
    final static String FIRST_COND_PANEL = "FirstCondition";
	final static String ADD_COND_PANEL = "AddCondition";
    int conditions=2;
    GradientPanel firstCondition=null;
    public static ArrayList<CmaesCondition> cmaesConditions;
    String firstCmaesCond;
    private File landscapeFile=null;
    GradientPanel coverPanel;
    JFrame parentFrame;
    
	public JFrame getParentFrame() {
		return parentFrame;
	}

	public void setParentFrame(JFrame parentFrame) {
		this.parentFrame = parentFrame;
	}

	public DialogSearchParams(JFrame parent,BiochamModelElement element,String title,JPanel pan) {
	
		
	    super (parent, true);	   
	    parentFrame=parent;
		boolean b=SwingUtilities.isEventDispatchThread();
		
	    modelElement=element;
	    dialogName=title;
	    model=modelElement.getModel();
	    panel=pan;
	    maxSpring=Spring.constant(30);
	    with_log=false;
	    initComponents(title);
	    pack();
	   
	     
	}

	private void initComponents(String title) {
		
		
		setTitle(title);
	    contents = getContentPane();
	    contents.setLayout(new BorderLayout());
	    coverPanel=new GradientPanel();
	    layout= new SpringLayout();
	    coverPanel.setLayout(layout);
	    contents.add(coverPanel,BorderLayout.CENTER);
	    params=((ParamTableParameters)model.getParameters().getParamTable()).parameters;
	    list = new String[params.size()];
	    for (int i=0;i<params.size();++i){
	         list[i] = params.get(i).getName();
	    }
	    //list[0] = "----";
	    bcomand="numerical_method(stiff).\n";
	    if(title.equals("Search Parameters") || title.equals("Search All Parameters") || title.equals("Search Random Parameters")){
	    	
	    	if(dialogName.equals("Search Parameters")){
				bcomand+="search_parameters";
			}else if(dialogName.equals("Search All Parameters")){
				bcomand+="search_all_parameters";
			}else if(dialogName.equals("Search Random Parameters")){
				bcomand+="search_random_parameters";
			}
	    	form(1);
	    
	    }else if(title.equals("Search Random All Parameters")){
	    	bcomand+="search_random_all_parameters";
	    	form(2);
	    	
	    }else if(title.equals("Search Parameters CMAES")){	 
	    	bcomand+="search_parameters_cmaes";
	    	cmaes_search_form(1);
	    	
	    }else if(title.equals("Robustness")){
	    	bcomand+="robustness";
	    	cmaes_search_form(2);
	    	
	    }else if(title.contains("Landscape")){
	    	bcomand+="landscape";
	    	cmaes_search_form(3);
	    	
	    }else if(title.equals("Search Parameters CMAES Multi Conditions")){
	    	bcomand+="cmaes_multi_conditions";
	    	cmaes_search_form(4);
	    }
	    
		
	    
	}
	
	private void cmaes_search_form(int t) {
		
		
		
		if(t==4){
			contents = getContentPane();
			contents.removeAll();
			contents.setLayout(new CardLayout());
			firstCondition=new GradientPanel();
			firstCondition.setLayout(layout);			
			cmaesConditions=new ArrayList<CmaesCondition>();
			
		}
		JLabel multiConditions = null;
		
		if(t==4){
			multiConditions=new JLabel("<html><u> First Condition: </u></html>");
			firstCondition.add(multiConditions);
		}
		final ButtonGroup group=new ButtonGroup();;	
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
		if(t==1 || t==4){
			txt=model.simulationMap.get("cmaesQuery");
			if(txt==null){
				txt=model.simulationMap.get("ltlQuery");
			}
		}else if(t==2){
			txt=model.simulationMap.get("robustnessQuery");
		}else{
			txt=model.simulationMap.get("landscapeQuery");
		}
		if(txt!=null){
			qfltlText.setText(txt);
		}
		
		JButton loadVar=new JButton("Load");
		JButton loadQuery=new JButton("Load");//load or write
		loadQuery.setName(Integer.toString(t));
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
								String t=((JButton)c).getName();
					            if(t.equals("1") || t.equals("4")){
					    			model.simulationMap.put("cmaesQuery",content);
					    		}else if(t.equals("2")){
					    			model.simulationMap.put("robustnessQuery",content);
					    		}else{
					    			model.simulationMap.put("landscapeQuery",content);
					    		}						
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
		if(t==4){
			
			firstCondition.add(qfltl);
			firstCondition.add(qfltlText);
			firstCondition.add(loadQuery);
		    firstCondition.add(b2);
		}else{
			coverPanel.add(qfltl);
			coverPanel.add(qfltlText);
			coverPanel.add(loadQuery);
			coverPanel.add(b2);
		}
	    if(t==4){
	    	layout.putConstraint(SpringLayout.WEST, multiConditions, 200, SpringLayout.WEST, firstCondition);
	 	    layout.putConstraint(SpringLayout.NORTH, multiConditions, 20,SpringLayout.NORTH, firstCondition);
		    layout.putConstraint(SpringLayout.WEST, qfltl, 15, SpringLayout.WEST, firstCondition);
		    layout.putConstraint(SpringLayout.NORTH, qfltl, 70,SpringLayout.NORTH, firstCondition);
		    
		    layout.putConstraint(SpringLayout.NORTH, qfltlText, 0,SpringLayout.NORTH, qfltl);
		    layout.putConstraint(SpringLayout.WEST, qfltlText, 5,SpringLayout.EAST, qfltl);
		    
		    layout.putConstraint(SpringLayout.WEST, loadQuery, 0,SpringLayout.WEST, loadVar);
		    layout.putConstraint(SpringLayout.NORTH, loadQuery, 65,SpringLayout.NORTH, firstCondition);
		    
		    layout.putConstraint(SpringLayout.WEST, b2, 2,SpringLayout.EAST, loadQuery);
		    layout.putConstraint(SpringLayout.NORTH, b2, 60,SpringLayout.NORTH, firstCondition);
		    
	    	
	    }else{
	    	layout.putConstraint(SpringLayout.WEST, qfltl, 15, SpringLayout.WEST, coverPanel);
	  	    layout.putConstraint(SpringLayout.NORTH, qfltl, 20,SpringLayout.NORTH, coverPanel);
	  	    layout.putConstraint(SpringLayout.NORTH, qfltlText, 0,SpringLayout.NORTH, qfltl);
		    layout.putConstraint(SpringLayout.WEST, qfltlText, 5,SpringLayout.EAST, qfltl);
		    
		    layout.putConstraint(SpringLayout.WEST, loadQuery, 5,SpringLayout.EAST, qfltlText);
		    layout.putConstraint(SpringLayout.NORTH, loadQuery, 0,SpringLayout.NORTH, qfltl);
		    
		    layout.putConstraint(SpringLayout.WEST, b2, 2,SpringLayout.EAST, loadQuery);
		    layout.putConstraint(SpringLayout.NORTH, b2, 0,SpringLayout.NORTH, qfltl);
	    }
	    	    
	   
	    
	    JLabel simTime=new JLabel("Simulation time:");
	    timeText=new JTextField(4);
	    txt=model.simulationMap.get("lastSim");
	    if(txt!=null){
	    	timeText.setText(txt);
	    }else{
	    	timeText.setText("20");
	    }
	    if(t==4){
	    		
	    	firstCondition.add(simTime);
	    	firstCondition.add(timeText);
	    }else{
	    	coverPanel.add(simTime);
	    	coverPanel.add(timeText);
	    }
	    
	    JLabel samples=null;
	    if(t==2 || t==3){
	    	samples=new JLabel("Samples:");
		    samplesText=new JTextField(4);
		    txt=model.simulationMap.get("samples");
		    if(txt!=null){
		    	samplesText.setText(txt);
		    }else{
		    	samplesText.setText("50");
		    }
		    coverPanel.add(samples);
		    coverPanel.add(samplesText);
	    }
	    
	    if(t==4){
	    	 layout.putConstraint(SpringLayout.WEST,simTime,15, SpringLayout.WEST,firstCondition);
	    }else{
	    	 layout.putConstraint(SpringLayout.WEST,simTime,15, SpringLayout.WEST,coverPanel);
	    }
	   
	    layout.putConstraint(SpringLayout.NORTH,simTime,20,SpringLayout.SOUTH,qfltl);
	    layout.putConstraint(SpringLayout.WEST,timeText, 10,SpringLayout.EAST,simTime);
	    layout.putConstraint(SpringLayout.NORTH,timeText, 0,SpringLayout.NORTH,simTime);
	    if(t==2 || t==3){
	    	layout.putConstraint(SpringLayout.WEST,samples,15, SpringLayout.EAST,timeText);
		    layout.putConstraint(SpringLayout.NORTH,samples,20,SpringLayout.SOUTH,qfltl);
		    layout.putConstraint(SpringLayout.WEST,samplesText, 10,SpringLayout.EAST,samples);
		    layout.putConstraint(SpringLayout.NORTH,samplesText, 0,SpringLayout.NORTH,samples);		   
	    }
	    
	    JLabel param = new JLabel("<html><u>Parameters:</u>");
	    if(t==4){
	    	
	    	firstCondition.add(param);
		    layout.putConstraint(SpringLayout.WEST, param, 15,SpringLayout.WEST, firstCondition);
	    }else{
	    	coverPanel.add(param);
		    layout.putConstraint(SpringLayout.WEST, param, 15,SpringLayout.WEST, coverPanel);
	    }
	    
	    layout.putConstraint(SpringLayout.NORTH, param, 40,SpringLayout.SOUTH, simTime);
	     
	    parameters=new ArrayList<String>();
	    for(int i=1;i<list.length;i++){
	    	parameters.add(list[i]);
	    }
	    
	    p1 = (new CustomComboBox(list,false)).getMultiSelectComboBox();    
	    Dimension d = p1.getPreferredSize();
	    p1.setPreferredSize(new Dimension(130, d.height));
	    p1.setPopupWidth(d.width+20);	  
	    p1.setName(Integer.toString(t));	    
	    final BalloonTip bTip=new BalloonTip(p1,"Don't forget to press ENTER to apply the selection.",Utils.modern,BalloonTip.Orientation.RIGHT_BELOW,BalloonTip.AttachLocation.SOUTHEAST,20,10,false);
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
        				CustomComboBox ccb=mcb.getParentClass();
        				ArrayList selected=ccb.getSelected();
        				
        				if(selected.size()!=0){
        					String nm=selected.get(0).toString();
        					boolean ok=false;
            				boolean allSelected=false;
            				bTip.setVisible(false);            				
            				ccb.setFinished(true);
            				mcb.setPopupVisible(false);
        					ArrayList all =ccb.getAllItems();
            				for(int i=0;i<selected.size();i++){
            					if(selected.get(i).equals("Select All")){
            						allSelected=true;
            						
            					}
            				}
            				if(allSelected){        					
            					selected=all;
            				}
            				String atext=pvaluesText.getText();
            				String items=chosenParams.getText();
            				int k=selected.size();   
            				 
            		        
            		        String t=mcb.getName();
            		  	    String newValue=null;
            		  	    if(t.equals("1") || t.equals("3") || t.equals("4")){        		  	    	  
            		  	    	newValue= (String)JOptionPane.showInputDialog(BiochamMainFrame.frame,"Enter the range of values for the chosen parameters:\n","Parameter Range");        		  	    	
            			        if(newValue!=null && !newValue.equals("Parameter Range")){        			        	
            			        	if(atext!=null && atext!=""){
            			        		if(atext.length()>0){
            			        			for(int i=0;i<k;i++){
            			        				atext+=",("+newValue+")";
            			        			}
            			        			pvaluesText.setText(atext);
            			        		}else{
            			        			atext="("+newValue+")";
            			        			for(int i=1;i<k;i++){
            			        				atext+=",("+newValue+")";
            			        			}
            			        			pvaluesText.setText(atext);
            			        		}
            			        	}
            			        	ok=true;
            			        }else{
            			        	ccb.desellectAll();
            			        }
            		  	    }else{        		  	    	
            		  	    	newValue= (String)JOptionPane.showInputDialog(BiochamMainFrame.frame,"Enter the coefficient of the variation of selected parameters:\n","Coefficient Variation");	
            		  	    	if(newValue!=null && !newValue.equals("Coefficient Variation")){
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
            				          ok=true;
            		  	    	}else{
            		  	    		ccb.desellectAll();
            		  	    	}
            		  	    }
            		  	    if(ok){
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
                		        CustomIconedCellRenderer rdr=ccb.getRenderer();    		      
                		        Vector selectedObjectsVector=rdr.getSelectedObjects();
                		        for(int i=0;i<selectedObjectsVector.size();i++){
                		        	Object o=selectedObjectsVector.get(i);
                		        	mcb.removeItem(o);
                		        	all.remove(o);
                		        }  
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
            		  	    	if(rest==1 && mcb.getItemAt(0).equals("Select All")){
            		  	    		mcb.setEnabled(false);        		  		  
            		  	    	}
            		  	    	rdr.unselectAll();        		  	
            		  	    	mcb.getParentClass().setFinished(false);
            		  	    }        		  	
        				}else{
        					CustomIconedCellRenderer rdr=mcb.getParentClass().getRenderer();	        
        		  	    	int rest=mcb.getItemCount();
        		  	    	if(rest==1 && mcb.getItemAt(0).equals("Select All")){
        		  	    		mcb.setEnabled(false);        		  		  
        		  	    	}
        		  	    	rdr.unselectAll();        		  	
        		  	    	mcb.getParentClass().setFinished(false);
        				}
        				
        			} 
        			bTip.setVisible(false);
        		}
        	}

	    });
	    
	    JLabel pvalues = new JLabel("<html><u>Values:</u>");
	    pvaluesText=new JTextField();
	    pvaluesText.setColumns(22);
	    pvaluesText.setEditable(false);
	    chosenParams=new JTextField(48);
	    chosenParams.setEditable(false);
	    
	    if(t==4){
	    	firstCondition.add(p1);
	    	firstCondition.add(pvalues);
	    	firstCondition.add(pvaluesText);
	    	firstCondition.add(chosenParams);
	    }else{
	    	coverPanel.add(p1);
	    	coverPanel.add(pvalues);
	    	coverPanel.add(pvaluesText);
	    	coverPanel.add(chosenParams);
	    }
	   
	    layout.putConstraint(SpringLayout.WEST, p1, 15,SpringLayout.EAST, param);
	    layout.putConstraint(SpringLayout.NORTH, p1, 35,SpringLayout.SOUTH, simTime);	    
	    layout.putConstraint(SpringLayout.WEST, pvalues, 15,SpringLayout.EAST, p1);
	    layout.putConstraint(SpringLayout.NORTH, pvalues, 0,SpringLayout.NORTH, param);	   
	    layout.putConstraint(SpringLayout.WEST, pvaluesText, 10,SpringLayout.EAST, pvalues);
	    layout.putConstraint(SpringLayout.NORTH, pvaluesText,37,SpringLayout.SOUTH, simTime);  
	    layout.putConstraint(SpringLayout.WEST, chosenParams, 0,SpringLayout.WEST, param);
	    layout.putConstraint(SpringLayout.NORTH, chosenParams,10,SpringLayout.SOUTH, param);
	    layout.putConstraint(SpringLayout.EAST, pvaluesText, 0,SpringLayout.EAST, b2);
	    layout.putConstraint(SpringLayout.EAST, chosenParams, 0,SpringLayout.EAST, b2);
	  	  	    
	    JLabel var=new JLabel("<html><u>Variables:</u>");
	   
	    
	    if(t==1 || t==4){
	    	txt=model.simulationMap.get("cmaesVars");
	    }else if(t==2){
	    	txt=model.simulationMap.get("robustnessVars");
	    }else{
	    	txt=model.simulationMap.get("landscapeVars");
	    }
	    
	    if(txt!=null){
	    	variables.setText(txt);
	    }	
		loadVar.setName(Integer.toString(t));
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
					            if(t.equals("1") || t.equals("4")){
					            	model.simulationMap.put("cmaesVars", content);
					    		}else if(t.equals("2")){
					    			model.simulationMap.put("robustnessVars",content);
					    		}else{
					    			model.simulationMap.put("landscapeVars",content);
					    		}						
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
		if(t==4){
			firstCondition.add(var);
			firstCondition.add(variables);
			firstCondition.add(loadVar);
			firstCondition.add(b3);
		}else{
			coverPanel.add(var);
			coverPanel.add(variables);
			coverPanel.add(loadVar);
			coverPanel.add(b3);
		}
	    layout.putConstraint(SpringLayout.WEST, var, 0, SpringLayout.WEST, chosenParams);
	    layout.putConstraint(SpringLayout.NORTH, var, 50,SpringLayout.SOUTH, chosenParams);  	    
	    layout.putConstraint(SpringLayout.WEST, variables, 0,SpringLayout.WEST, p1);
	    layout.putConstraint(SpringLayout.NORTH, variables, 50,SpringLayout.SOUTH, chosenParams);    
	    layout.putConstraint(SpringLayout.WEST, loadVar, 15,SpringLayout.EAST, variables);
	    layout.putConstraint(SpringLayout.NORTH, loadVar, 45,SpringLayout.SOUTH, chosenParams);
	    layout.putConstraint(SpringLayout.WEST, b3, 2,SpringLayout.EAST, loadVar);
	    layout.putConstraint(SpringLayout.NORTH, b3, 35,SpringLayout.SOUTH, chosenParams);
	    
	    JLabel val=new JLabel("<html><u>Values:</u>");
	   
	    if(t==1 || t==4){
	    	txt=model.simulationMap.get("cmaesVals");
	    }else if(t==2){
	    	txt=model.simulationMap.get("robustnessVals");
	    }else{
	    	txt=model.simulationMap.get("landscapeVals");
	    }
	    
	   
	    if(txt!=null){
	    	varValues.setText(txt);
	    }
		JButton loadVal=new JButton("Load");
		loadVal.setName(Integer.toString(t));
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
									String t=((JButton)c).getName();
						            if(t.equals("1") || t.equals("4")){
						            	model.simulationMap.put("cmaesVals", content);
						    		}else if(t.equals("2")){
						    			model.simulationMap.put("robustnessVals",content);
						    		}else{
						    			model.simulationMap.put("landscapeVals",content);
						    		}	
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
		
		if(t==4){
			firstCondition.add(val);
			firstCondition.add(varValues);
			firstCondition.add(loadVal);
			firstCondition.add(b4);
		}else{
			coverPanel.add(val);
			coverPanel.add(varValues);
			coverPanel.add(loadVal);
			coverPanel.add(b4);
		}
		
	    layout.putConstraint(SpringLayout.WEST, val, 0, SpringLayout.WEST, var);
	    layout.putConstraint(SpringLayout.NORTH, val, 20,SpringLayout.SOUTH, var);  	    
	    layout.putConstraint(SpringLayout.WEST, varValues, 0,SpringLayout.WEST, p1);
	    layout.putConstraint(SpringLayout.NORTH, varValues, 0,SpringLayout.NORTH, val);    
	    layout.putConstraint(SpringLayout.WEST, loadVal, 15,SpringLayout.EAST, varValues);
	    layout.putConstraint(SpringLayout.NORTH, loadVal, 15,SpringLayout.SOUTH, var);
	    layout.putConstraint(SpringLayout.WEST, b4, 2,SpringLayout.EAST, loadVal);
	    layout.putConstraint(SpringLayout.NORTH, b4, 5,SpringLayout.SOUTH, var);
	    
	    JCheckBox cmaesParams=null;
	    
	    JLabel iter=null,fitness=null, coefOfvariation=null,seedL=null;
	    if(t==1){
		    cmaesParams=new JCheckBox("CMAES stop criteria:");
		    cmaesParams.setSelected(false);
		    cmaesParams.setOpaque(false);
		    cmaesParams.addActionListener(new ActionListener(){
	
				public void actionPerformed(ActionEvent e) {
					JCheckBox cb=(JCheckBox)e.getSource();
					if (cb.isSelected()){
						 iteration.setEditable(true);
						 objFitness.setEditable(true);
						 variation.setEditable(true);
					}else{
						iteration.setEditable(false);
						objFitness.setEditable(false);
						variation.setEditable(false);
					}
					
				}});
		    
		    iter=new JLabel("Iteration:");
		    coefOfvariation=new JLabel("Coeff. of Variation:");
		    fitness=new JLabel("Stop Fitness:");
		    iteration=new JTextField(5);
		    variation=new JTextField(5);
		    
		    seedL=new JLabel("Seed: ");
		    seedTf=new JTextField(5);
		    seedTf.setEditable(true);
		    txt= model.simulationMap.get("seed");
		    if(txt!=null){
		    	seedTf.setText(txt);		    	
		    }else{
		    	seedTf.setText("0");
		    }
		    txt= model.simulationMap.get("variation");
		    if(txt!=null){
		    	variation.setText(txt);		    	
		    }else{
		    	variation.setText("0.1");
		    }
		    variation.setEditable(false);
		    txt= model.simulationMap.get("cmaesIter");
		    if(txt!=null){
		    	iteration.setText(txt);		    	
		    }else{
		    	iteration.setText("300");
		    }
		    iteration.setEditable(false);
		    objFitness=new JTextField(5);
		    txt= model.simulationMap.get("cmaesFitness");
		    if(txt!=null){
		    	objFitness.setText(txt);
		    }else{
		    	objFitness.setText("0.01");
		    }
		    objFitness.setEditable(false);
		    coverPanel.add(cmaesParams);
		    coverPanel.add(iter);
		    coverPanel.add(fitness);
		    coverPanel.add(coefOfvariation);
		    coverPanel.add(variation);
		    coverPanel.add(iteration);
		    coverPanel.add(objFitness);
		    coverPanel.add(seedL);
		    coverPanel.add(seedTf);
	    }

	    JRadioButton lognormals2 = null;
	    if(t!=4){
		    JRadioButton lognormals1=new JRadioButton("Lognormals distribution");
		    lognormals1.setSelected(false);
		    lognormals1.setOpaque(false);
		    lognormals1.addActionListener(new ActionListener(){
	
				public void actionPerformed(ActionEvent e) {
					JRadioButton cb=(JRadioButton)e.getSource();
					if (cb.getText().contains("Lognormals") && cb.isSelected()){
						with_log=true;
					}else{
						with_log=false;
					}
					
				}});
		    lognormals2=new JRadioButton("Normal distribution");
		    lognormals2.setSelected(true);
		    lognormals2.setOpaque(false);
		    lognormals2.addActionListener(new ActionListener(){
	
				public void actionPerformed(ActionEvent e) {
					JRadioButton cb=(JRadioButton)e.getSource();
					if (cb.getText().contains("Normal") && cb.isSelected()){
						with_log=true;
					}else{
						with_log=false;
					}
					
				}});
		
		    
		    group.add(lognormals1);
		    group.add(lognormals2);
		    coverPanel.add(lognormals1);
		    coverPanel.add(lognormals2);
		    
		    if(t==1){
			    layout.putConstraint(SpringLayout.WEST, cmaesParams, 0, SpringLayout.WEST, var);
			    layout.putConstraint(SpringLayout.NORTH, cmaesParams, 40,SpringLayout.SOUTH, val);  
			    
			    layout.putConstraint(SpringLayout.WEST, lognormals1, 166, SpringLayout.EAST, cmaesParams);
			    layout.putConstraint(SpringLayout.NORTH, lognormals1, 43,SpringLayout.SOUTH, val);  
			    layout.putConstraint(SpringLayout.WEST, lognormals2, 166, SpringLayout.EAST, cmaesParams);
			    layout.putConstraint(SpringLayout.NORTH, lognormals2, 2,SpringLayout.SOUTH, lognormals1); 
			   	    
			    layout.putConstraint(SpringLayout.WEST, coefOfvariation, 0, SpringLayout.WEST, var);
			    layout.putConstraint(SpringLayout.NORTH, coefOfvariation, 60,SpringLayout.SOUTH, cmaesParams);		    
			    layout.putConstraint(SpringLayout.WEST, variation, 10, SpringLayout.EAST, coefOfvariation);
			    layout.putConstraint(SpringLayout.NORTH, variation, 60,SpringLayout.SOUTH, cmaesParams); 
			    
			    layout.putConstraint(SpringLayout.WEST, fitness, 0, SpringLayout.WEST, var);
			    layout.putConstraint(SpringLayout.NORTH, fitness, 40,SpringLayout.SOUTH, cmaesParams);  
			    layout.putConstraint(SpringLayout.WEST, objFitness, 0, SpringLayout.WEST, variation);
			    layout.putConstraint(SpringLayout.NORTH, objFitness, 40,SpringLayout.SOUTH, cmaesParams); 
			   	    
			    layout.putConstraint(SpringLayout.WEST, iter, 0, SpringLayout.WEST, var);
			    layout.putConstraint(SpringLayout.NORTH, iter, 20,SpringLayout.SOUTH, cmaesParams);  
			    layout.putConstraint(SpringLayout.WEST, iteration, 0, SpringLayout.WEST, variation);
			    layout.putConstraint(SpringLayout.NORTH, iteration, 20,SpringLayout.SOUTH, cmaesParams); 
			    
			    layout.putConstraint(SpringLayout.WEST, seedL, 0, SpringLayout.WEST, var);
			    layout.putConstraint(SpringLayout.NORTH, seedL, 80,SpringLayout.SOUTH, cmaesParams);  
			    layout.putConstraint(SpringLayout.WEST, seedTf, 0, SpringLayout.WEST, variation);
			    layout.putConstraint(SpringLayout.NORTH, seedTf, 80,SpringLayout.SOUTH, cmaesParams); 
			    
			    
		    }else{
		    	layout.putConstraint(SpringLayout.WEST, lognormals1, 0, SpringLayout.WEST, var);
			    layout.putConstraint(SpringLayout.NORTH, lognormals1, 40,SpringLayout.SOUTH, val);  
			    layout.putConstraint(SpringLayout.WEST, lognormals2, 0, SpringLayout.WEST, var);
			    layout.putConstraint(SpringLayout.NORTH, lognormals2, 2,SpringLayout.SOUTH, lognormals1);		      
		    }
	    }
	   
	    
	    
	    JButton search=new JButton("Search");
	    if(t==1){
	    	search.setActionCommand("CMD_SEARCH");
	    }else if(t==2){
	    	search.setActionCommand("ROBUSTNESS");
	    }else if(t==3){
	    	search.setActionCommand("LANDSCAPE");
	    }
	    search.addActionListener(this);
	    JButton cancel=new JButton("Cancel");
	    cancel.setActionCommand(CMD_CANCEL);
	    cancel.addActionListener(this);
	    
	    if(t==4){
	    	firstCondition.add(search);
	    	firstCondition.add(cancel);
	    }else{
	    	coverPanel.add(search);
	    	coverPanel.add(cancel);
	    }
	   
	    if(t==1){
		    layout.putConstraint(SpringLayout.WEST, search, 220, SpringLayout.WEST, coverPanel);
		    layout.putConstraint(SpringLayout.NORTH, search, 70,SpringLayout.SOUTH, objFitness);  
		    layout.putConstraint(SpringLayout.WEST, cancel, 10, SpringLayout.EAST, search);
		    layout.putConstraint(SpringLayout.NORTH, cancel, 70,SpringLayout.SOUTH, objFitness); 
	    }else if(t!=4 && t!=1){
	    	layout.putConstraint(SpringLayout.WEST, search, 220, SpringLayout.WEST, coverPanel);
		    layout.putConstraint(SpringLayout.NORTH, search, 50,SpringLayout.SOUTH, lognormals2);  
		    layout.putConstraint(SpringLayout.WEST, cancel, 10, SpringLayout.EAST, search);
		    layout.putConstraint(SpringLayout.NORTH, cancel, 50,SpringLayout.SOUTH, lognormals2); 
	    }else if(t==4){
	    	search.setText("Add Conditions");
	    	search.setActionCommand("addConditions");
	    	layout.putConstraint(SpringLayout.WEST, search, 180, SpringLayout.WEST, firstCondition);
		    layout.putConstraint(SpringLayout.NORTH, search, 70,SpringLayout.SOUTH, varValues);  
		    layout.putConstraint(SpringLayout.WEST, cancel, 10, SpringLayout.EAST, search);
		    layout.putConstraint(SpringLayout.NORTH, cancel, 70,SpringLayout.SOUTH, varValues); 
	    }
	    JFrame frame=BiochamMainFrame.frame;
	    if(t==4){
	    	contents.add(firstCondition,FIRST_COND_PANEL);
	    	CmaesConditionPanel condPanel=new CmaesConditionPanel(this,conditions,modelElement, list);	  
	    	contents.add(condPanel,ADD_COND_PANEL);
	    	CardLayout cl = (CardLayout)(contents.getLayout());
  	  		cl.show(contents, FIRST_COND_PANEL);
	    }
	    Point pos = frame.getLocationOnScreen();
	    setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);
	    setResizable(false);
	    if(Utils.is_OS_MAC){
	    	setSize(new Dimension(650,560));
	    }else{
	    	setSize(new Dimension(600,560));
	    }
	    
	   
	    setLocationRelativeTo(frame);
	    setVisible(true);		
	      
	      
	}

	
	public void addNewConditionPanel() {
	
		   	  
		conditions++;
		CmaesConditionPanel condPanel=new CmaesConditionPanel(this,conditions,modelElement, list);	  
		contents.add(condPanel,ADD_COND_PANEL);
    	CardLayout cl = (CardLayout)(contents.getLayout());
	  	cl.show(contents, ADD_COND_PANEL);
	}
	
	public void startSearch() {
		
	
		String multiConds="";
   	  	for(int i=0;i<cmaesConditions.size();i++){
   	  		multiConds+="add_search_condition("+cmaesConditions.get(i).getQfltlQuery()+","+cmaesConditions.get(i).getListOfVariables()+","+
   	  		cmaesConditions.get(i).getListOfValues()+","+cmaesConditions.get(i).getLsitOfParametersModifications()+").\n";
   	  	}
   	  	String bcCommand=firstCmaesCond+multiConds+"cmaes_multi_conditions.\n";
   	  	Utils.debugMsg("Final multi command is: \n"+bcCommand);
   	 
   	  	result=bcCommand;	
   	  	setVisible(false);
   	  	cmaesConditions.clear();
   	  	firstCmaesCond=null;
   	    dispose();
	    cmaesConditions.clear();
	}

	
	
	
	
	
	private void form(int c){
		
		JLabel timeLabel = null,stepsLabel=null,h1=null,h2=null,h3=null;		
		Dimension d=null;
		code=c;
		if(code==1 || code==2){	
		
		
		  ltlLabel = new JLabel("LTL Specification: ");
		  if(((ParamTableLTLSpecifications)model.getLtlSpecifications().getParamTable()).getLtlModel().getSpecifications().size()==0){
			  if(model.simulationMap.get("ltlQuery")==null){
				  ltlText = new TextFieldLTL(parentFrame,"QFLTL","double click to compose",modelElement);
			  }else{
				  ltlText = new TextFieldLTL(parentFrame,"QFLTL",model.simulationMap.get("ltlQuery"),modelElement);
			  }
		  }else{
			  if(model.simulationMap.get("ltlQuery")==null){
				  ltlText = new TextFieldLTL(parentFrame,"QFLTL","[optional]",modelElement);
			  }else{
				  ltlText = new TextFieldLTL(parentFrame,"QFLTL",model.simulationMap.get("ltlQuery"),modelElement);
			  }
			 
		  }
		 
		  ltlText.setColumns(20);
		  coverPanel.add(ltlLabel);
		  coverPanel.add(ltlText);
	      layout.putConstraint(SpringLayout.WEST, ltlLabel, 15, SpringLayout.WEST, coverPanel);
	      layout.putConstraint(SpringLayout.NORTH, ltlLabel, 20,SpringLayout.NORTH, coverPanel);
	      layout.putConstraint(SpringLayout.WEST, ltlText, 5,SpringLayout.EAST, ltlLabel);
	      layout.putConstraint(SpringLayout.NORTH, ltlText, 0,SpringLayout.NORTH, ltlLabel);

	      timeLabel = new JLabel("Simulation time: ");
	      String txt1 = model.simulationMap.get("lastSim");	     
	      timeText = new JTextField(4);
	      if(txt1!=null){
	    	  timeText.setText(txt1);
	      }else{
	    	  timeText.setText("20");
	      }

	      stepsLabel = new JLabel("Enumeration steps: ");
	      stepsText = new JTextField(3);
	      txt1=model.simulationMap.get("enumSteps");
	      if(txt1!=null){
	    	  stepsText.setText(txt1);
	      }else{
	    	  stepsText.setText("10");
	      }
	      coverPanel.add(timeLabel);
	      layout.putConstraint(SpringLayout.WEST, timeLabel, 15,SpringLayout.WEST, coverPanel);
	      layout.putConstraint(SpringLayout.NORTH, timeLabel, 15,SpringLayout.SOUTH, ltlLabel);
	      coverPanel.add(timeText);
	      layout.putConstraint(SpringLayout.WEST, timeText, 12,SpringLayout.EAST, timeLabel);
	      layout.putConstraint(SpringLayout.NORTH, timeText, 0,SpringLayout.NORTH, timeLabel);
	      coverPanel.add(stepsLabel);
	      layout.putConstraint(SpringLayout.NORTH, stepsLabel, 0,SpringLayout.NORTH, timeLabel);
	      coverPanel.add(stepsText);
	      layout.putConstraint(SpringLayout.EAST, stepsLabel, -5,SpringLayout.WEST, stepsText);
	      layout.putConstraint(SpringLayout.EAST, stepsText, 0,SpringLayout.EAST, ltlText);
	      layout.putConstraint(SpringLayout.NORTH, stepsText, 0,SpringLayout.NORTH, stepsLabel);
	      
		}
		
//	    Middle
		
		if(code==1){
			
			
			 JLabel param = new JLabel("<html><u>Parameters:</u>");
			 coverPanel.add(param);
			 layout.putConstraint(SpringLayout.WEST, param, 15,SpringLayout.WEST, coverPanel);
			 layout.putConstraint(SpringLayout.NORTH, param, 30,SpringLayout.SOUTH, timeLabel);
			     
			    parameters=new ArrayList<String>();
			    for(int i=1;i<list.length;i++){
			    	parameters.add(list[i]);
			    }
			    
			    p1 = (new CustomComboBox(list,false)).getMultiSelectComboBox();	     
			    d = p1.getPreferredSize();
			    p1.setPreferredSize(new Dimension(130, d.height));
			    p1.setPopupWidth(d.width+20);	
			    
			    final BalloonTip bTip=new BalloonTip(p1,"Don't forget to press ENTER to apply the selection.",Utils.modern,BalloonTip.Orientation.RIGHT_BELOW,BalloonTip.AttachLocation.SOUTHEAST,20,10,false);
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
		        				
		        				boolean allSelected=false,go_ahead=true;
		        				CustomMultiSelectComboBox mcb=(CustomMultiSelectComboBox)e.getSource();		        				
		        				ArrayList selected=mcb.getParentClass().getSelected();
		        				
		        				if(selected.size()>2){		        							        					
			        				bTip.setVisible(false);
			        				mcb.setPopupVisible(false);	
			        				bTip.setVisible(false);
		        					int k=JOptionPane.showConfirmDialog(BiochamMainFrame.frame,"<html>For the performance ressons, it is recommended that the maximum number of <br>" +
		        							" parameters is not higher then 2. Do you still want to continue?<br><br></html>","Recommendation",JOptionPane.YES_NO_OPTION);
		        					if(k==JOptionPane.NO_OPTION){
		        						go_ahead=false;
		        						mcb.getParentClass().desellectAll();
		        					}else{
		        						go_ahead=true;
		        					}
		        				}else if(selected.size()==0){
		        					go_ahead=false;
		        					
		        				}
		        				if(go_ahead){
		        					
		        					mcb.getParentClass().setFinished(true);
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
			        		  	    
			        		  	    newValue= (String)JOptionPane.showInputDialog(BiochamMainFrame.frame,"Enter the range of values for the chosen parameters:\n","Parameter Range");			        		  	    	
			        		  	    if(newValue!=null && !newValue.equals("Parameter Range")){        			        	
			        		  	    	if(atext!=null && atext!=""){
			        		  	    		if(atext.length()>0){
			        		  	    			for(int i=0;i<k;i++){
			        		  	    				atext+=",("+newValue+")";
			        		  	    			}
			        		  	    			pvaluesText.setText(atext);
			        		  	    		}else{
			        		  	    			atext="("+newValue+")";
			        		  	    			for(int i=1;i<k;i++){
			        		  	    				atext+=",("+newValue+")";
			        		  	    			}
			        		  	    			pvaluesText.setText(atext);
			        		  	    		}
			        		  	    	}
			        		  	    	
			        		  	    	if(items!=null && items!=""){
				        		        	if(items.length()>0){
					        		        	for(int i=0;i<selected.size();i++){
					        		        		items+=","+selected.get(i);        		        		
					        		       		}
					        		        	StringTokenizer st=new StringTokenizer(items,",");
					        		        	if(st.countTokens()==2 || st.countTokens()>2){
					        		        		JOptionPane.showMessageDialog(BiochamMainFrame.frame,"<html>For the performance ressons, it is recommended that the maximum number of <br>" +
						        							" parameters is not higher then 2.<br><br></html>","Recommendation",JOptionPane.INFORMATION_MESSAGE);
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
				        		              		        
				        		        CustomIconedCellRenderer rdr=mcb.getParentClass().getRenderer();	        
				        		        Vector selectedObjectsVector=rdr.getSelectedObjects();
				        		        for(int i=0;i<selectedObjectsVector.size();i++){
				        		        	mcb.removeItem(selectedObjectsVector.get(i));
				        		        	mcb.getParentClass().getAllItems().remove(selectedObjectsVector.get(i));
				        		        }
				        		        
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
					        		     if(rest==1 && mcb.getItemAt(0).equals("Select All")){
					        		    	 mcb.setEnabled(false);        		  		  
					        		     }
					        		  	 rdr.unselectAll();        		  	
					        		  	 mcb.getParentClass().setFinished(false);
			        		  	    }
			        			}else{
			        				
		        		  	    	 CustomIconedCellRenderer rdr=mcb.getParentClass().getRenderer();	        
				        		     int rest=mcb.getItemCount();
				        		     if(rest==1 && mcb.getItemAt(0).equals("Select All")){
				        		    	 mcb.setEnabled(false);        		  		  
				        		     }
				        		  	 rdr.unselectAll();        		  	
				        		  	 mcb.getParentClass().setFinished(false);			        		  	    
			        			}   
		        				bTip.setVisible(false);
		        			}
		        			bTip.setVisible(false);
		        		}
		        		bTip.setVisible(false);
		        	}	
			    });
			    
			    coverPanel.add(p1);
			    layout.putConstraint(SpringLayout.WEST, p1, 10,SpringLayout.EAST, param);
			    layout.putConstraint(SpringLayout.NORTH, p1, 25,SpringLayout.SOUTH, timeLabel);
			    
			    JLabel pvalues = new JLabel("<html><u>Values:</u>");
			    coverPanel.add(pvalues);
			    layout.putConstraint(SpringLayout.WEST, pvalues, 15,SpringLayout.WEST, coverPanel);
			    layout.putConstraint(SpringLayout.NORTH, pvalues, 15,SpringLayout.SOUTH, param);
			    
			    pvaluesText=new JTextField();
			    pvaluesText.setColumns(23);
			    pvaluesText.setEditable(false);
			    coverPanel.add(pvaluesText);
			    layout.putConstraint(SpringLayout.WEST, pvaluesText, 0,SpringLayout.WEST, p1);
			    layout.putConstraint(SpringLayout.NORTH, pvaluesText,12,SpringLayout.SOUTH, param);
			     
			    chosenParams=new JTextField(31);
			    chosenParams.setEditable(false);
			    coverPanel.add(chosenParams);
			    layout.putConstraint(SpringLayout.WEST, chosenParams, 0,SpringLayout.WEST, pvalues);
			    layout.putConstraint(SpringLayout.NORTH, chosenParams,25,SpringLayout.SOUTH, pvalues);
			
		}else if(code==2){
			
			  h2 = new JLabel("From");
			  coverPanel.add(h2);
		      layout.putConstraint(SpringLayout.WEST, h2, 15,SpringLayout.WEST, coverPanel);
		      layout.putConstraint(SpringLayout.NORTH, h2, 30,SpringLayout.SOUTH, timeLabel);
		      
		      t12 = new JTextField(5);
		      String txt=model.simulationMap.get("from");
		      if(txt!=null){
		    	  t12.setText(txt);
		      }else{
		    	  t12.setText("");
		      }
		      coverPanel.add(t12);
		      layout.putConstraint(SpringLayout.WEST, t12, 0,SpringLayout.WEST, h2);
		      layout.putConstraint(SpringLayout.NORTH, t12, 10,SpringLayout.SOUTH, h2);
		      
		      h3 = new JLabel("To");
		      coverPanel.add(h3);
		      layout.putConstraint(SpringLayout.WEST, h3, 45,SpringLayout.EAST, t12);
		      layout.putConstraint(SpringLayout.NORTH, h3, 0,SpringLayout.NORTH, h2);
		      
		      t13 = new JTextField(5);
		      txt=model.simulationMap.get("to");
		      if(txt!=null){
		    	  t13.setText(txt);
		      }else{
		    	  t13.setText("");
		      }
		      coverPanel.add(t13);
		      layout.putConstraint(SpringLayout.WEST, t13, 0,SpringLayout.WEST, h3);
		      layout.putConstraint(SpringLayout.NORTH, t13, 0,SpringLayout.NORTH, t12);
			
		}
	      
	     	      
	      //Buttons
	      
	      // cancel button
	      JButton cancelButton = new JButton();
	      cancelButton.setText("Cancel");
	      cancelButton.setActionCommand(CMD_CANCEL);
	      cancelButton.addActionListener(this);
	      JButton okButton = new JButton();
	      okButton.setText("Search");
	      okButton.setActionCommand(CMD_OK);
	      okButton.addActionListener(this);
	      coverPanel.add(okButton);     
	      coverPanel.add(cancelButton);
	      
	      
	      if(code==1){ 
	    	  	layout.putConstraint(SpringLayout.WEST, okButton, 130, SpringLayout.WEST, contents);
			    layout.putConstraint(SpringLayout.NORTH, okButton, 50,SpringLayout.SOUTH, chosenParams);  
			    layout.putConstraint(SpringLayout.WEST, cancelButton, 10, SpringLayout.EAST, okButton);
			    layout.putConstraint(SpringLayout.NORTH, cancelButton, 50,SpringLayout.SOUTH, chosenParams);
	      }else if(code==2){
	    	  layout.putConstraint(SpringLayout.EAST, cancelButton, 0,SpringLayout.EAST, stepsText);
	    	  layout.putConstraint(SpringLayout.NORTH, cancelButton, 60,SpringLayout.SOUTH, t13);
	      }
	      
	      // ok button
	      if(code==2){
	    	  layout.putConstraint(SpringLayout.EAST, okButton, -13,SpringLayout.WEST, cancelButton);
	    	  layout.putConstraint(SpringLayout.NORTH, okButton, 60,SpringLayout.SOUTH, t12);
	      } 
	      
	      JFrame frame=BiochamMainFrame.frame;
	      Point pos = frame.getLocationOnScreen();
	      setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);
	      setResizable(false);
	      if(code==1){ 
	    	  setSize(new Dimension(400,350));
	      }else if(code==2){
	    	  setSize(new Dimension(400,300));
	      }
	      setLocationRelativeTo(frame);
	      setVisible(true);		 
	}
	
	
	public String getResult() {
		return result;
	}

	public void setResult(String result) {
		this.result = result;
	}

	public void actionPerformed(ActionEvent e) {

		
		 
	      boolean closeWindow = false;
	      String cmd = e.getActionCommand();
	      if (cmd == null) {
	         // do nothing
	      } else if (cmd.equals(CMD_CANCEL)) {
	          result = null;
	          closeWindow = true;
	      } else if (cmd.equals(CMD_OK)) {
	    	  
	    	  //clearResultsFile();	    	   
	    	  String error="";
	    	  boolean b=SwingUtilities.isEventDispatchThread();
	          try {
	        	  StringBuffer buf;
	        	  if(code==1){
	        		
		             buf = new StringBuffer(bcomand+"([");
		             if(chosenParams.getText().length()>0 && chosenParams.getText()!=""){
		            	 buf.append(chosenParams.getText()+"],");
		            	 if(pvaluesText.getText().length()>0 && pvaluesText.getText()!=""){
		            		 buf.append("["+pvaluesText.getText()+"],");		                   
		            		 error = stepsText.getText();
		            		 model.simulationMap.put("enumSteps", error);
		            		 int steps = Integer.parseInt(error);		               
		            		 if(ltlText.getText().equals("[optional]") || ltlText.getText()==null || ltlText.getText().length()<1 || ltlText.getText()==""){		                	
		            			 buf.append(steps+", ");
		            		 }else{
		            			 buf.append(steps+", "+ltlText.getText()+", ");
		            			 model.simulationMap.put("ltlQuery",ltlText.getText());
		            		 }
		            		 error = timeText.getText();
		            		 model.simulationMap.put("lastSim", error);
		            		 float time = Float.parseFloat(error);
		            		 buf.append(time+").\n");	
		            		 result = buf.toString();
		            	 } else {
		            		 result = null;
		            	 }
		             }else{
		            	 result = null;
		             }		  
		            	 
	        	  }else if(code==2){
	        		  buf = new StringBuffer("search_random_all_parameters(");
	        		  model.simulationMap.put("from", t12.getText());
	        		  model.simulationMap.put("to", t13.getText());
	        		  model.simulationMap.put("enumSteps", stepsText.getText());
	        		  model.simulationMap.put("lastSim", timeText.getText());
	        		  float f=Float.parseFloat(t12.getText());
	        		  /*if(f==0){
	        			  f+=0.01;
	        		  }*/
			          buf.append(f+","+Float.parseFloat(t13.getText())+","+Integer.parseInt(stepsText.getText())+",");
			          if(ltlText.getText().equals("[optional]") || ltlText.getText()==null || ltlText.getText().length()<1 || ltlText.getText()==""){
			          }else{
		                	buf.append(ltlText.getText()+", ");
		                	model.simulationMap.put("ltlQuery",ltlText.getText());
			          }
			          buf.append(Integer.parseInt(timeText.getText())+").\n");	
			          result = buf.toString();
	        	  }
	        	  
	             closeWindow = true;
	             
	          } catch  (Exception excp) {
	             JOptionPane.showMessageDialog(this,
	                   "Invalid value: "+error,
	                   "Invalid value",
	                   JOptionPane.WARNING_MESSAGE);
	          }
	    	  closeWindow = true;
	      }else if(cmd.equals("addConditions")){
	    	
	    	  bcomand="first_search_condition";
	    	  StringBuffer buf= new StringBuffer(bcomand+"([");
	    	  if(chosenParams.getText().length()>0 && chosenParams.getText()!=""){
	        	  buf.append(chosenParams.getText()+"],");
	        	  if(pvaluesText.getText().length()>0 && pvaluesText.getText()!=""){
	        		  buf.append("["+pvaluesText.getText()+"],");
	        		  if(qfltlText.getText().length()>0 && qfltlText.getText()!=""){
	        			  buf.append(qfltlText.getText()+",");
	        			  model.simulationMap.put("cmaesQuery",qfltlText.getText());
	        			  if(variables.getText().length()>0 && variables.getText()!=""){
	        				  model.simulationMap.put("cmaesVars", variables.getText());
	        				  buf.append("["+variables.getText()+"],");
	        				  if(varValues.getText().length()>0 && varValues.getText()!=""){
	        					  buf.append("["+varValues.getText()+"],"+timeText.getText()+").\n");
	        					  model.simulationMap.put("cmaesVals", varValues.getText());
	        					  model.simulationMap.put("lastSim", timeText.getText());
	        					  result = buf.toString();
	        				  }else{
	        					  result = null;
	        				  }
	        			  }else{
	        				  result = null;
	        			  }
	        		  }else{
	        			  result = null;
	        		  }
	        	  }else{
	        		  result = null;
	        	  }
	          }else{
	        	  result = null;
	          }
	    	  
	    	  if(result!=null){
	    		  firstCmaesCond=result;
	    		  CardLayout cl = (CardLayout)(contents.getLayout());
		    	  cl.show(contents, ADD_COND_PANEL);
	    	  }else{
	    		  JOptionPane.showMessageDialog(this,"You've made an error while completing the form.","Warning",JOptionPane.WARNING_MESSAGE);
	    		  CardLayout cl = (CardLayout)(contents.getLayout());
		    	  cl.show(contents, FIRST_COND_PANEL);
	    	  }
	    	  
	    	  
	      }else if(cmd.equals("multiCondCmaesSearch")){
	    	  //start the search method.......
	    	  String multiConds="";
	    	  for(int i=0;i<cmaesConditions.size();i++){
	    		  multiConds+="add_search_condition("+cmaesConditions.get(i).getQfltlQuery()+","+cmaesConditions.get(i).getListOfVariables()+","+
	    		  cmaesConditions.get(i).getListOfValues()+","+cmaesConditions.get(i).getLsitOfParametersModifications()+").\n";
	    	  }
	    	  String bcCommand=firstCmaesCond+".\n"+multiConds+"cmaes_multi_conditions.\n";
	    	  Utils.debugMsg("Final multi command is: \n"+bcCommand);
	    	  //model.sendToBiocham(bcCommand, "");
	    	  result=bcCommand;
	    	  setVisible(false);
	    	  cmaesConditions.clear();
	    	  firstCmaesCond=null;
	    	  dispose();
	      }else if(cmd.equals("addAnotherSearchCondition")){
	    	  //save values, and then...
	    	 /* contents.add(firstCondition,FIRST_COND_PANEL);
		    	contents.add(addCondition,ADD_COND_PANEL);
		    	CardLayout cl = (CardLayout)(contents.getLayout());
	  	  		cl.show(contents, FIRST_COND_PANEL);*/
	    	 // addCondition=createConditionPanel();
	    	  conditions++;
	    	  CmaesConditionPanel condPanel=new CmaesConditionPanel(this,conditions,modelElement, list);	  
	    	  contents.add(condPanel,ADD_COND_PANEL);
	    	  CardLayout cl = (CardLayout)(contents.getLayout());
	    	  cl.show(contents, ADD_COND_PANEL);
	      }
	      else if(cmd.equals("CMD_SEARCH")){
	    	  	
	    	  //clearResultsFile();
	    	  String error="";
	    	  boolean b=SwingUtilities.isEventDispatchThread();
	          try {
	        	  bcomand="cmaes_params("+Integer.parseInt(iteration.getText())+","+Float.parseFloat(objFitness.getText())+","+Float.parseFloat(variation.getText())+").\n";
	        	  model.simulationMap.put("cmaesIter", iteration.getText());
	        	  model.simulationMap.put("cmaesFitness", objFitness.getText());
	        	  model.simulationMap.put("variation", variation.getText());
	        	  if(!seedTf.getText().equals("0")){
	        		  bcomand+="seed("+Integer.parseInt(seedTf.getText())+").\n";
	        		  model.simulationMap.put("seed", seedTf.getText());
	        	  }
	        	  
	        	  if(with_log){	        		  
	        		  bcomand+="search_parameters_cmaes_log";
	        	  }else{
	        		  bcomand+="search_parameters_cmaes";
	        	  }
	        	  
	        	  StringBuffer buf= new StringBuffer(bcomand+"([");
		          if(chosenParams.getText().length()>0 && chosenParams.getText()!=""){
		        	  buf.append(chosenParams.getText()+"],");
		        	  if(pvaluesText.getText().length()>0 && pvaluesText.getText()!=""){
		        		  buf.append("["+pvaluesText.getText()+"],");
		        		  if(qfltlText.getText().length()>0 && qfltlText.getText()!=""){
		        			  buf.append(qfltlText.getText()+",");
		        			  model.simulationMap.put("cmaesQuery",qfltlText.getText());
		        			  if(variables.getText().length()>0 && variables.getText()!=""){
		        				  model.simulationMap.put("cmaesVars", variables.getText());
		        				  buf.append("["+variables.getText()+"],");
		        				  if(varValues.getText().length()>0 && varValues.getText()!=""){
		        					  buf.append("["+varValues.getText()+"],"+timeText.getText()+").\n");
		        					  model.simulationMap.put("cmaesVals", varValues.getText());
		        					  model.simulationMap.put("lastSim", timeText.getText());
		        					  result = buf.toString();
		        				  }else{
		        					  result = null;
		        				  }
		        			  }else{
		        				  result = null;
		        			  }
		        		  }else{
		        			  result = null;
		        		  }
		        	  }else{
		        		  result = null;
		        	  }
		          }else{
		        	  result = null;
		          }
		          closeWindow = true;
	             
	          } catch  (Exception excp) {
	             JOptionPane.showMessageDialog(this,
	                   "Invalid value: "+error,
	                   "Invalid value",
	                   JOptionPane.WARNING_MESSAGE);
	          }
	    	  closeWindow = true;
	    	  
	    	  
	      }else if(cmd.equals("ROBUSTNESS")){

	    	 // clearResultsFile();
	    	  String error="";
	    	  bcomand="";
	          try {
	        	  
	        	  if(with_log){
	        		  bcomand="robustness_log";
	        	  }else{
	        		  bcomand="robustness";
	        	  }
	        	  
	        	  StringBuffer buf= new StringBuffer(bcomand+"([");
		          if(chosenParams.getText().length()>0 && chosenParams.getText()!=""){
		        	  buf.append(chosenParams.getText()+"],");
		        	  if(pvaluesText.getText().length()>0 && pvaluesText.getText()!=""){
		        		  buf.append("["+pvaluesText.getText()+"],");
		        		  if(qfltlText.getText().length()>0 && qfltlText.getText()!=""){
		        			  buf.append(qfltlText.getText()+",");
		        			  model.simulationMap.put("robustnessQuery",qfltlText.getText());
		        			  if(variables.getText().length()>0 && variables.getText()!=""){
		        				  model.simulationMap.put("robustnessVars", variables.getText());
		        				  buf.append("["+variables.getText()+"],");
		        				  if(varValues.getText().length()>0 && varValues.getText()!=""){
		        					  buf.append("["+varValues.getText()+"],"+samplesText.getText()+","+timeText.getText()+").\n");
		        					  model.simulationMap.put("robustnessVals", varValues.getText());
		        					  model.simulationMap.put("lastSim", timeText.getText());
		        					  model.simulationMap.put("samples", samplesText.getText());
		        					  result = buf.toString();
		        				  }else{
		        					  result = null;
		        				  }
		        			  }else{
		        				  result = null;
		        			  }
		        		  }else{
		        			  result = null;
		        		  }
		        	  }else{
		        		  result = null;
		        	  }
		          }else{
		        	  result = null;
		          }
		          closeWindow = true;
	             
	          } catch  (Exception excp) {
	             JOptionPane.showMessageDialog(this,
	                   "Invalid value: "+error,
	                   "Invalid value",
	                   JOptionPane.WARNING_MESSAGE);
	          }
	    	  closeWindow = true;
	    	  
	    	  
	      
	    	  
	      }else if(cmd.equals("LANDSCAPE")){
	    	  
	    	  //clearResultsFile();
	    	  String error="";
	    	  bcomand="";
	          try {
	        	  
	        	  if(with_log){
	        		  bcomand="landscape_log";
	        	  }else{
	        		  bcomand="landscape";
	        	  }
	        	  
	        	  StringBuffer buf= new StringBuffer(bcomand+"([");
		          if(chosenParams.getText().length()>0 && chosenParams.getText()!=""){
		        	
		        	  StringTokenizer st=new StringTokenizer(chosenParams.getText(),",");
		        	  if(st.countTokens()>2){
		        		  JOptionPane.showMessageDialog(BiochamMainFrame.frame, "You have to specify exactly 2 parameters. See Biocham Documentation for more help.","Warning",JOptionPane.WARNING_MESSAGE);
		        	  }else if(st.countTokens()<2){
		        		  JOptionPane.showMessageDialog(BiochamMainFrame.frame, "You have to specify exactly 2 parameters. See Biocham Documentation for more help.","Warning",JOptionPane.WARNING_MESSAGE);
		        	  }else{
		        	  
			        	  buf.append(chosenParams.getText()+"],");
			        	  if(pvaluesText.getText().length()>0 && pvaluesText.getText()!=""){
			        		  buf.append("["+pvaluesText.getText()+"],");
			        		  if(qfltlText.getText().length()>0 && qfltlText.getText()!=""){
			        			  buf.append(qfltlText.getText()+",");
			        			  model.simulationMap.put("landscapeQuery",qfltlText.getText());
			        			  if(variables.getText().length()>0 && variables.getText()!=""){
			        				  model.simulationMap.put("landscapeVars", variables.getText());
			        				  buf.append("["+variables.getText()+"],");
			        				  if(varValues.getText().length()>0 && varValues.getText()!=""){
			        					  buf.append("["+varValues.getText()+"],"+samplesText.getText()+","+timeText.getText()+",");
			        					  File file=null;
			        					  String rep=Utils.showSaveDialog("",BiochamMainFrame.frame,SupportedSuffixes.CSV_SUFFIX);			
			        					  if (rep!=null) {
			        						  file=new File(rep);
			        					  }else{
			        						  file=new File(Utils.getNameWithoutExtension(model.getModelFile().getName()+"_LANDSCAPE"));
			        					  }
			        					  buf.append("'"+file.getAbsolutePath()+"'"+").\n");
			        					  setLandscapeFile(file);
			        					  // ASK HERE FOR FILE FOR LANDSCAPE.........
			        					  
			        					  model.simulationMap.put("landscapeVals", varValues.getText());
			        					  model.simulationMap.put("lastSim", timeText.getText());
			        					  model.simulationMap.put("samples", samplesText.getText());
			        					  result = buf.toString();
			        				  }else{
			        					  result = null;
			        				  }
			        			  }else{
			        				  result = null;
			        			  }
			        		  }else{
			        			  result = null;
			        		  }
			        	  }else{
			        		  result = null;
			        	  }
		        	  }
		          }else{
		        	  result = null;
		          }
		          closeWindow = true;
		          
	          } catch  (Exception excp) {
	             JOptionPane.showMessageDialog(this,
	                   "Invalid value: "+error,
	                   "Invalid value",
	                   JOptionPane.WARNING_MESSAGE);
	          }
	    	  closeWindow = true;
	    	  
	      }
	      else {
	         closeWindow = true;
	      }
	      if (closeWindow) {
	         setVisible(false);
	         dispose();
	      }
	   
		
	}

	public File getLandscapeFile() {
		return landscapeFile;
	}

	public void setLandscapeFile(File lanscapeFile) {
		this.landscapeFile = lanscapeFile;
	}
	
	 
	 
}

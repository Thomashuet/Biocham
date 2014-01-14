package fr.inria.contraintes.biocham.dialogs;


import fr.inria.contraintes.biocham.modelData.TopLeftCTLPanelFirst;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.BiochamModelElement;
import fr.inria.contraintes.biocham.customComponents.GradientPanel;
import fr.inria.contraintes.biocham.customComponents.TextFieldCTL;
import fr.inria.contraintes.biocham.customComponents.TextFieldLTL;
import fr.inria.contraintes.biocham.customComponents.TextFieldRules;
import fr.inria.contraintes.biocham.customComponents.UniversalScrollPane;
import fr.inria.contraintes.biocham.modelData.MacrosModel;
import fr.inria.contraintes.biocham.modelData.MoleculesModel;
import fr.inria.contraintes.biocham.modelData.ParamTableLTLSpecifications;
import fr.inria.contraintes.biocham.modelData.ParamTableMacros;
import fr.inria.contraintes.biocham.modelData.ParamTableMolecules;
import fr.inria.contraintes.biocham.modelData.ParamTableRules;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.Spring;
import javax.swing.SpringLayout;
import javax.swing.SwingUtilities;



public class DialogOptions extends JDialog implements ActionListener{

	
	   private final Font commonFont = new Font("",Font.PLAIN,12);
	   private String CMD_CANCEL = "cmd.cancel";
	   private String CMD_OK = "cmd.ok",CMD_ANALYZE="cmd.analyze",CMD_NUSMV="cmd.nusmv";	   
	   private  String name,value="";	   
	   private BiochamModelElement modelElement;
	   JTextField nameText,valueText,eventText,stepsDoubling;
	   JLabel nameLabel,valueLabel,eventLabel,nameLabel2;
	   JCheckBox reviseOption;
	   String result = null;
	   ArrayList<String> options;
	   static JList list;
	   JPanel panel;
	   boolean label=true;
	   boolean event=false;
	   final int numButtons = 8;
	   final ButtonGroup group=new ButtonGroup();	   
	   final String defaultMessageCommand = "default";
       final String yesNoCommand = "yesno";
       final String yeahNahCommand = "yeahnah";
       final String yncCommand = "ync";
       JRadioButton[] radioButtons;
       JRadioButton addition,deletion;
       boolean add=false,delete=false;
       BiochamModel model;
       Spring maxSpring;
       String dialogName;
       Container contents;
       GradientPanel coverPanel;
       UniversalScrollPane listScroller,listScroller1;
       SpringLayout layout;
             
       JTextField timeText,stepsText,convFText,threshold;
       JTextField xminT,xmaxT,yminT,ymaxT,initFromTraceT;
       boolean maxT=false,minT=false,periodT=false;
       GradientPanel p;
       JFrame parentFrame;
       
	   public DialogOptions(JFrame parent,BiochamModelElement element,String title,JPanel pan) {
		   //DialogOptions od=new DialogOptions(BiochamMainFrame.frame,model,"Numerical Simulation");
	      super (parent, true);	   
	      parentFrame=parent;
	      
	  	  boolean b=SwingUtilities.isEventDispatchThread();
	  	
	  	
	      modelElement=element;
	      dialogName=title;
	      model=modelElement.getModel();
	      panel=pan;
	      maxSpring=Spring.constant(30);
	     
	      initComponents(title);
	      
	      pack();
	   }
	   
	   public DialogOptions(JFrame parent,BiochamModel m,String title) {
		   
	      super (parent, true);	   
	      parentFrame=parent;
	  	  boolean b=SwingUtilities.isEventDispatchThread();
	  	
	      dialogName=title;
	      model=m;	      
	      maxSpring=Spring.constant(30);
	      initComponents(title);	      
	      pack();
	   }
	   
	   private void initComponents(String n) {
	     
	      // initialize the window
	      setTitle(n);
	      contents = getContentPane();
	      contents.setLayout(new BorderLayout());
	      coverPanel=new GradientPanel();
	      layout= new SpringLayout();
	      coverPanel.setLayout(layout);
	      contents.add(coverPanel,BorderLayout.CENTER);
	      
	     	      
	      if(n.equals("Simulation")){
	    	  nameLabel=new JLabel("<html><u>Method:</u>");
	    	  coverPanel.add(nameLabel);
	    	  radioButtons = new JRadioButton[4];	
	    	  radioButtons[0]=new JRadioButton("Runge-Kutta");
	    	  radioButtons[0].setActionCommand("rk");    	 
	    	  radioButtons[1]=new JRadioButton("Rosenbrock");
	    	  radioButtons[1].setActionCommand("stiff");	    	
	    	  radioButtons[2]=new JRadioButton("Gillespie");
	    	  radioButtons[2].setActionCommand("ssa");
	    	  radioButtons[3]=new JRadioButton("Tau-Leaping");
	    	  radioButtons[3].setActionCommand("tl");
	    	  
	    	  for (int i = 0; i < 4; i++) {
	    		  radioButtons[i].addActionListener(this);
		    	  group.add(radioButtons[i]);		    	  
		    	  coverPanel.add(radioButtons[i]);		    	  
		      }
	    	  String txt=model.simulationMap.get("numMethod");
	    	  if(txt!=null){
	    		  int j=Integer.parseInt(txt);
	    		  radioButtons[j].setSelected(true);  
	    	  }else{
	    		  radioButtons[1].setSelected(true);
	    	  }	      
		      layout.putConstraint(SpringLayout.WEST, nameLabel, 15, SpringLayout.WEST,coverPanel);
		      layout.putConstraint(SpringLayout.NORTH, nameLabel, 30, SpringLayout.NORTH,coverPanel);
		      layout.putConstraint(SpringLayout.WEST, radioButtons[0], 15, SpringLayout.WEST,coverPanel);
		      layout.putConstraint(SpringLayout.NORTH, radioButtons[0], 5, SpringLayout.SOUTH,nameLabel);
		      layout.putConstraint(SpringLayout.WEST, radioButtons[1], 10, SpringLayout.EAST,radioButtons[0]);
		      layout.putConstraint(SpringLayout.WEST, radioButtons[2], 10, SpringLayout.EAST,radioButtons[1]);
		      layout.putConstraint(SpringLayout.WEST, radioButtons[3], 10, SpringLayout.EAST,radioButtons[2]);
		      layout.putConstraint(SpringLayout.NORTH, radioButtons[1], 0, SpringLayout.NORTH,radioButtons[0]);
		      layout.putConstraint(SpringLayout.NORTH, radioButtons[2], 0, SpringLayout.NORTH,radioButtons[0]);
		      layout.putConstraint(SpringLayout.NORTH, radioButtons[3], 0, SpringLayout.NORTH,radioButtons[0]);
		      
		      
		      JLabel time=new JLabel("Time:");
		      coverPanel.add(time);		      
		      timeText=new JTextField(4);
		      txt=model.simulationMap.get("lastSim");
		      if(txt!=null){
		    	  timeText.setText(txt);
		      }else{
		    	  timeText.setText("20");
		      }
		      coverPanel.add(timeText);
		      JLabel steps=new JLabel("Step size:");
		     
		      coverPanel.add(steps);
		      stepsText=new JTextField(4);
		      txt=model.simulationMap.get("numSteps");
		      if(txt!=null){
		    	  stepsText.setText(txt);
		      }else{
		    	  stepsText.setText("0.01");
		      }
		      coverPanel.add(stepsText);
		      JCheckBox steps_doubling=new JCheckBox("Steps Doubling:");
		      steps_doubling.setSelected(false);
		      steps_doubling.addActionListener(new ActionListener(){

				public void actionPerformed(ActionEvent e) {
					Component c=(Component)e.getSource();
					if(c instanceof JCheckBox){
						JCheckBox cb=(JCheckBox)c;
						if(cb.isSelected()){
							stepsDoubling.setEditable(true);
						}else{
							stepsDoubling.setEditable(false);
						}
					}
					
				}});
		      coverPanel.add(steps_doubling);
		      stepsDoubling=new JTextField(4);
		      txt=model.simulationMap.get("stepsDoubling");
		      if(txt!=null){
		    	  stepsDoubling.setText(txt);
		      }else{
		    	  stepsDoubling.setText("0.0001");
		      }
		      stepsDoubling.setEditable(false);
		      coverPanel.add(stepsDoubling);
		      JLabel convFactor=new JLabel("Conversion Factor:");
		      coverPanel.add(convFactor);
		      convFText=new JTextField(5);
		      
		      if(radioButtons[0].isSelected() || radioButtons[1].isSelected()){
		    	  convFText.setEditable(false);
		    	  convFText.setText("");
		      }else{
		    	  convFText.setEditable(true);
		    	  txt=model.simulationMap.get("convFact");
			      if(txt!=null){
			    	  convFText.setText(txt);
			      }else{
			    	  convFText.setText("10000");
			      }
		      }
		      coverPanel.add(convFText);
		      JLabel crThreshold=new JLabel("Critical Reaction Threshold:");
		      coverPanel.add(crThreshold);
		      threshold=new JTextField(4); 
		      if(radioButtons[0].isSelected() || radioButtons[1].isSelected()){
		    	 	 
		    	  threshold.setText("");
		    	  threshold.setEditable(false);	
		      }else{
		    	  threshold.setEditable(true);
		    	  txt=model.simulationMap.get("threshold");
			      if(txt!=null){
			    	  threshold.setText(txt);
			      }else{
			    	  threshold.setText("20");
			      }
		      }
		     
		      coverPanel.add(threshold);
		      
		      layout.putConstraint(SpringLayout.WEST, time, 20, SpringLayout.WEST,coverPanel);
		      layout.putConstraint(SpringLayout.NORTH, time, 15, SpringLayout.SOUTH,radioButtons[0]);
		      layout.putConstraint(SpringLayout.WEST, timeText, 10, SpringLayout.EAST,time);
		      layout.putConstraint(SpringLayout.NORTH, timeText, 0, SpringLayout.NORTH,time);
		      layout.putConstraint(SpringLayout.WEST, steps, 20, SpringLayout.EAST,timeText);
		      layout.putConstraint(SpringLayout.NORTH, steps, 0,SpringLayout.NORTH,time);
		      layout.putConstraint(SpringLayout.WEST, stepsText, 10, SpringLayout.EAST,steps);
		      layout.putConstraint(SpringLayout.NORTH, stepsText, 0, SpringLayout.NORTH,steps);		      
		      layout.putConstraint(SpringLayout.WEST, convFactor, 20, SpringLayout.EAST,stepsText);
		      layout.putConstraint(SpringLayout.NORTH, convFactor, 0,SpringLayout.NORTH,time);
		      layout.putConstraint(SpringLayout.WEST, convFText, 10, SpringLayout.EAST,convFactor);
		      layout.putConstraint(SpringLayout.NORTH, convFText, 0, SpringLayout.NORTH,convFactor);	      
		      
		      layout.putConstraint(SpringLayout.WEST, steps_doubling, 13, SpringLayout.WEST,coverPanel);
		      layout.putConstraint(SpringLayout.NORTH, steps_doubling, 35,SpringLayout.SOUTH,time);
		      layout.putConstraint(SpringLayout.WEST, stepsDoubling, 0, SpringLayout.WEST,threshold);
		      layout.putConstraint(SpringLayout.NORTH, stepsDoubling, 37,SpringLayout.SOUTH,time);
		      
		      layout.putConstraint(SpringLayout.WEST, crThreshold, 15, SpringLayout.WEST,coverPanel);
		      layout.putConstraint(SpringLayout.NORTH, crThreshold, 7,SpringLayout.SOUTH,steps_doubling);
		      layout.putConstraint(SpringLayout.WEST, threshold, 20, SpringLayout.EAST,crThreshold);
		      layout.putConstraint(SpringLayout.NORTH, threshold, 7,SpringLayout.SOUTH,steps_doubling);
		      
		      JLabel set=new JLabel("<html><u>Range of the numerical plot:</u>");
		      coverPanel.add(set);
		      layout.putConstraint(SpringLayout.WEST, set, 15, SpringLayout.WEST,coverPanel);
		      layout.putConstraint(SpringLayout.NORTH, set, 35,SpringLayout.SOUTH,crThreshold);
		      JLabel xmin,xmax,ymin,ymax,initFromTrace;
		      
		      xmin=new JLabel("xmin:");
		      xmax=new JLabel("xmax:");
		      ymin=new JLabel("ymin:");
		      ymax=new JLabel("ymax:");
		      initFromTrace=new JLabel("Initial state:");
		      xminT=new JTextField(3);
		      txt=model.simulationMap.get("xmin");
		      if(txt!=null){
		    	  xminT.setText(txt);
		      }
		      xmaxT=new JTextField(3);
		      txt=model.simulationMap.get("xmax");
		      if(txt!=null){
		    	  xmaxT.setText(txt);
		      }
		      yminT=new JTextField(3);
		      txt=model.simulationMap.get("ymin");
		      if(txt!=null){
		    	  yminT.setText(txt);
		      }
		      ymaxT=new JTextField(3);
		      txt=model.simulationMap.get("ymax");
		      if(txt!=null){
		    	  ymaxT.setText(txt);
		      }
		      initFromTraceT=new JTextField(3);
		      txt=model.simulationMap.get("initFromTrace");
		      if(txt!=null){
		    	  initFromTraceT.setText(txt);
		      }
		      coverPanel.add(xmin);
		      coverPanel.add(xminT);
		      coverPanel.add(xmax);
		      coverPanel.add(xmaxT);
		      coverPanel.add(ymin);
		      coverPanel.add(yminT);
		      coverPanel.add(ymax);
		      coverPanel.add(ymaxT);
		      
		      coverPanel.add(initFromTrace);
		      coverPanel.add(initFromTraceT);
		      
		      layout.putConstraint(SpringLayout.WEST, xmin, 0, SpringLayout.WEST,set);
		      layout.putConstraint(SpringLayout.NORTH, xmin, 10,SpringLayout.SOUTH,set);
		      layout.putConstraint(SpringLayout.WEST, xminT, 5, SpringLayout.EAST,xmin);
		      layout.putConstraint(SpringLayout.NORTH, xminT, 0,SpringLayout.NORTH,xmin);		      
		      layout.putConstraint(SpringLayout.WEST, xmax, 10, SpringLayout.EAST,xminT);
		      layout.putConstraint(SpringLayout.NORTH, xmax, 0,SpringLayout.NORTH,xmin);
		      layout.putConstraint(SpringLayout.WEST, xmaxT, 5, SpringLayout.EAST,xmax);
		      layout.putConstraint(SpringLayout.NORTH, xmaxT, 0,SpringLayout.NORTH,xmax);		      
		      layout.putConstraint(SpringLayout.WEST, ymin, 10, SpringLayout.EAST,xmaxT);
		      layout.putConstraint(SpringLayout.NORTH, ymin, 0,SpringLayout.NORTH,xmin);
		      layout.putConstraint(SpringLayout.WEST, yminT, 5, SpringLayout.EAST,ymin);
		      layout.putConstraint(SpringLayout.NORTH, yminT, 0,SpringLayout.NORTH,ymin);		      
		      layout.putConstraint(SpringLayout.WEST, ymax, 10, SpringLayout.EAST,yminT);
		      layout.putConstraint(SpringLayout.NORTH, ymax, 0,SpringLayout.NORTH,xmin);
		      layout.putConstraint(SpringLayout.WEST, ymaxT, 5, SpringLayout.EAST,ymax);
		      layout.putConstraint(SpringLayout.NORTH, ymaxT, 0,SpringLayout.NORTH,ymax);		      
		      layout.putConstraint(SpringLayout.WEST, initFromTrace, 15, SpringLayout.WEST,coverPanel);
		      layout.putConstraint(SpringLayout.NORTH, initFromTrace, 20,SpringLayout.SOUTH,xmin);
		      layout.putConstraint(SpringLayout.WEST, initFromTraceT, 5, SpringLayout.EAST,initFromTrace);
		      layout.putConstraint(SpringLayout.NORTH, initFromTraceT, 0,SpringLayout.NORTH,initFromTrace);
		      
		     
		      JLabel mol=new JLabel("<html><u>Show molecules:</u>");     
		      coverPanel.add(mol);
		      
		      MoleculesModel ptMolecules=((ParamTableMolecules)model.getMolecules().getParamTable()).getMoleculesModel();
		      int molSize=ptMolecules.getMolecules().size();
		      
		      String molecules[]=new String[molSize];
		      for(int h=0;h<molecules.length;h++){
		    	  molecules[h]=ptMolecules.getMolecules().get(h);
		      }
		      JList molList=new JList(molecules);
		      molList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		      molList.setLayoutOrientation(JList.VERTICAL);
		      molList.setVisibleRowCount(1);
		      molList.setToolTipText("Tip: Ctrl+Click on the item to deselect it.");		      
		      listScroller = new UniversalScrollPane(molList);
		      listScroller.setPreferredSize(new Dimension(200, 100));
		      coverPanel.add(listScroller);
		    
		      layout.putConstraint(SpringLayout.WEST, mol, 15, SpringLayout.WEST,coverPanel);
		      layout.putConstraint(SpringLayout.NORTH, mol, 30,SpringLayout.SOUTH,initFromTrace);
		      layout.putConstraint(SpringLayout.WEST, listScroller, 15, SpringLayout.WEST,coverPanel);
		      layout.putConstraint(SpringLayout.NORTH, listScroller, 10,SpringLayout.SOUTH,mol);
		      
		      JLabel macro=new JLabel("<html><u>Show macros:</>");     
		      coverPanel.add(macro);
		      
		      MacrosModel ptMacros=((ParamTableMacros)model.getMacros().getParamTable()).getMacrosModel();
		      
		      int macrosSize=ptMacros.getMacros().size();
		      
		      String macros[]=new String[macrosSize];
		      for(int h=0;h<macros.length;h++){
		    	  macros[h]=((String) ptMacros.getMacros().get(h));
		      }
		      JList macroList=new JList(macros);
		      macroList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		      macroList.setLayoutOrientation(JList.VERTICAL);
		      macroList.setVisibleRowCount(1);
		      macroList.setToolTipText("Tip: Ctrl+Click on the item to deselect it.");		      
		      listScroller1 = new UniversalScrollPane(macroList);
		      listScroller1.setPreferredSize(new Dimension(200, 100));
		      coverPanel.add(listScroller1);
		    
		      layout.putConstraint(SpringLayout.WEST, macro, 0, SpringLayout.WEST,listScroller1);
		      layout.putConstraint(SpringLayout.NORTH, macro, 30,SpringLayout.SOUTH,initFromTrace);
		      layout.putConstraint(SpringLayout.WEST, listScroller1, 15, SpringLayout.EAST,listScroller);
		      layout.putConstraint(SpringLayout.NORTH, listScroller1, 10,SpringLayout.SOUTH,macro);
		     	     
		      
		      JCheckBox maxTrace=new JCheckBox("Show maximum from the trace");
		      maxTrace.setSelected(false);
		      maxTrace.addActionListener(new ActionListener(){

				public void actionPerformed(ActionEvent e) {
					Component c=(Component)e.getSource();
					if(c instanceof JCheckBox){
						JCheckBox cb=(JCheckBox)c;
						if(cb.isSelected()){
							maxT=true;
						}else{
							maxT=false;
						}
					}
					
				}});
		      JCheckBox minTrace=new JCheckBox("Show minimum from the trace");
		      minTrace.setSelected(false);
		      minTrace.addActionListener(new ActionListener(){

				public void actionPerformed(ActionEvent e) {
					Component c=(Component)e.getSource();
					if(c instanceof JCheckBox){
						JCheckBox cb=(JCheckBox)c;
						if(cb.isSelected()){
							minT=true;
						}else{
							minT=false;
						}
					}
					
				}});
		      JCheckBox periodTrace=new JCheckBox("Show period from the trace");
		      periodTrace.setSelected(false);
		      periodTrace.addActionListener(new ActionListener(){

				public void actionPerformed(ActionEvent e) {
					Component c=(Component)e.getSource();
					if(c instanceof JCheckBox){
						JCheckBox cb=(JCheckBox)c;
						if(cb.isSelected()){
							periodT=true;
						}else{
							periodT=false;
						}
					}
					
				}});
		      coverPanel.add(maxTrace);
		      coverPanel.add(minTrace);
		      coverPanel.add(periodTrace);
		      
		      layout.putConstraint(SpringLayout.WEST, maxTrace, 15, SpringLayout.WEST,coverPanel);
		      layout.putConstraint(SpringLayout.NORTH, maxTrace, 10,SpringLayout.SOUTH,listScroller);
		      layout.putConstraint(SpringLayout.WEST, minTrace, 15, SpringLayout.WEST,coverPanel);
		      layout.putConstraint(SpringLayout.NORTH, minTrace, 1,SpringLayout.SOUTH,maxTrace);
		      layout.putConstraint(SpringLayout.WEST, periodTrace, 15, SpringLayout.WEST,coverPanel);
		      layout.putConstraint(SpringLayout.NORTH, periodTrace, 1,SpringLayout.SOUTH,minTrace);
		      
		      JButton okB=new JButton("Run");
		      okB.setActionCommand("NumSim");	
		      okB.addActionListener(new ActionListener() { public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      coverPanel.add(okB);
		      
		      JButton cancelB=new JButton("Cancel");
		      cancelB.setActionCommand(CMD_CANCEL);		    
		      cancelB.addActionListener(new ActionListener() { public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      coverPanel.add(cancelB);
		      
		      layout.putConstraint(SpringLayout.WEST, okB, 40, SpringLayout.WEST,xmaxT);
		      layout.putConstraint(SpringLayout.NORTH, okB, 35,SpringLayout.SOUTH,periodTrace);
		      layout.putConstraint(SpringLayout.WEST, cancelB, 15, SpringLayout.EAST,okB);
		      layout.putConstraint(SpringLayout.NORTH, cancelB, 0,SpringLayout.NORTH,okB);
		      
		      JFrame frame=parentFrame;
		      Point pos = frame.getLocationOnScreen();
		      setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);
		      setResizable(false);
		      setSize(new Dimension(500, 650));	    
		      setLocationRelativeTo(frame);
		      setVisible(true);	      
		      
	    	  
	      }

	      if(n.equals("Search Parameters")){
	    	  
		      nameLabel = new JLabel("<html><b><u>Choose the method of searching for parameter values:</u></b>");
		      nameLabel.setFont(Utils.rulesFont);
		      coverPanel.add(nameLabel);
		      
		      radioButtons = new JRadioButton[numButtons];	
		      JLabel[] labels = new JLabel[numButtons];
		      
		      
		      radioButtons[0] = new JRadioButton("<html><i>Search Parameters</i>");
		      radioButtons[0].setOpaque(false);
		      JLabel l1=new JLabel("<html> -  Gives the first values found for the parameters demanded.<html>");
		      l1.setFont(commonFont);
		      labels[0]=l1;
		      radioButtons[0].setActionCommand("search_parameters");
		      
		      
	          radioButtons[1] = new JRadioButton("<html><i>Search All Parameters</i>");
	          radioButtons[1].setOpaque(false);
	          JLabel l2=new JLabel(" - Gives all the values found for the parameters satisfying the LTL specification.");
	          l2.setFont(commonFont);
		      labels[1]=l2;
		      radioButtons[1].setActionCommand("search_all_parameters");
		     
		      
		      radioButtons[2] = new JRadioButton("<html><i>Search Random Parameters</i>");
		      //"<html>&nbsp - A greater number of parameter values can be searched with this command<P>"+"&nbsp &nbsp&nbsp for LTL containing many solutions."
		      JLabel l3=new JLabel("<html> - Gives the first values found by making random choices of values in the given <br>" +
		      								 " &nbsp &nbsp list of intervals for each parameter.</html>");
		      radioButtons[2].setOpaque(false);
		      l3.setFont(commonFont);
		      labels[2]=l3;
		      radioButtons[2].setActionCommand("search_random_parameters");
		    
		      
	          radioButtons[3] = new JRadioButton("<html><i>Search Random All Parameters</i>");
	          radioButtons[3].setOpaque(false);
	          JLabel l4=new JLabel("<html>&nbsp - Searches the values for all parameters of the current model in a given <P>"+"&nbsp &nbsp&nbsp interval of possible values and gives the first value found.");
	          l4.setFont(commonFont);
		      labels[3]=l4;
	          radioButtons[3].setActionCommand("search_random_all_parameters");
		     
		      
		      radioButtons[4] = new JRadioButton("<html><i>Search Parameters CMAES</i>");
		      radioButtons[4].setOpaque(false);
		      JLabel l5=new JLabel("<html>&nbsp - Uses the violation degree of the given temporal specification as a fitness <P>"+"&nbsp &nbsp&nbsp function for the non-linear optimization tool cmaes to guide the search.");
		      l5.setFont(commonFont);
		      labels[4]=l5;
		      radioButtons[4].setActionCommand("search_parameters_cmaes");
		   
		      
		      radioButtons[5] = new JRadioButton("<html><i>Search Parameters CMAES Multi Conditions</i>");
		      radioButtons[5].setOpaque(false);
		      JLabel l6=new JLabel("<html>&nbsp - Searches for parameter values satisfying different specifications in multiple<br>&nbsp &nbsp&nbsp conditions.</html>");
		      l6.setFont(new Font("",Font.PLAIN,12));
		      labels[5]=l6;
		      radioButtons[5].setActionCommand("cmaes_multi_conditions");
		    
		      
		     /* radioButtons[6] = new JRadioButton("<html><i>Compute robustness</i>");
		      JLabel l7=new JLabel("<html>&nbsp - Computes the robustness and relative robustness of the system with <P>"+"&nbsp &nbsp&nbsp respect to the specification given as QFLTL formula.");
		      l7.setFont(commonFont);
		      labels[6]=l7;
		      radioButtons[6].setActionCommand("robustness");
		    
		      
		      radioButtons[7] = new JRadioButton("<html><i>Compute the satisfaction degree landscape</i>");
		      JLabel l8=new JLabel("<html>&nbsp - Displays the satisfaction degree landscape of a QFLTL formula <P>"+"&nbsp &nbsp&nbsp on a parameter grid.");
		      l8.setFont(commonFont);
		      labels[7]=l8;
		      radioButtons[7].setActionCommand("landscape");*/
		   
		      
		      //JLabel l=null;
		      int size=6;
		      for (int i = 0; i < size; i++) {
		    	  group.add(radioButtons[i]);
		    	//  if(i==6){l=new JLabel("________________________________________________________________________________________");contents.add(l);}		    	  	
		    	  coverPanel.add(radioButtons[i]);
		    	  coverPanel.add(labels[i]);	
		      }
		      radioButtons[4].setSelected(true);
		      
		      layout.putConstraint(SpringLayout.WEST, nameLabel, 50,SpringLayout.WEST, coverPanel);
		      layout.putConstraint(SpringLayout.NORTH, nameLabel, 30, SpringLayout.NORTH, coverPanel);
		      
		      layout.putConstraint(SpringLayout.NORTH, radioButtons[4], 90, SpringLayout.NORTH, coverPanel);	      
		   	  layout.putConstraint(SpringLayout.WEST, radioButtons[4], 10,SpringLayout.WEST, coverPanel);
		   	  layout.putConstraint(SpringLayout.NORTH, labels[4], 110, SpringLayout.NORTH, coverPanel);	      
		   	  layout.putConstraint(SpringLayout.WEST, labels[4], 20,SpringLayout.WEST, coverPanel);	
		   	  
		   	  layout.putConstraint(SpringLayout.NORTH, radioButtons[5],10, SpringLayout.SOUTH, labels[4]);	   	      
		   	  layout.putConstraint(SpringLayout.WEST, radioButtons[5], 10,SpringLayout.WEST, coverPanel);
		   	  layout.putConstraint(SpringLayout.NORTH, labels[5], 30, SpringLayout.SOUTH, labels[4]);		      
		   	  layout.putConstraint(SpringLayout.WEST, labels[5], 20,SpringLayout.WEST, coverPanel);	
		   	  
		   	  layout.putConstraint(SpringLayout.NORTH, radioButtons[1], 10, SpringLayout.SOUTH, labels[0]);	      
		   	  layout.putConstraint(SpringLayout.WEST, radioButtons[1], 10,SpringLayout.WEST, coverPanel);
		   	  layout.putConstraint(SpringLayout.NORTH, labels[1], 30, SpringLayout.SOUTH, labels[0]);	      
		   	  layout.putConstraint(SpringLayout.WEST, labels[1], 20,SpringLayout.WEST, coverPanel);
		   	  
		   	  layout.putConstraint(SpringLayout.NORTH, radioButtons[2], 10, SpringLayout.SOUTH, labels[1]);	      
		   	  layout.putConstraint(SpringLayout.WEST, radioButtons[2], 10,SpringLayout.WEST, coverPanel);
		   	  layout.putConstraint(SpringLayout.NORTH, labels[2], 30, SpringLayout.SOUTH, labels[1]);	      
		   	  layout.putConstraint(SpringLayout.WEST, labels[2], 20,SpringLayout.WEST, coverPanel);
		   	  
		   	  layout.putConstraint(SpringLayout.NORTH, radioButtons[3], 10, SpringLayout.SOUTH, labels[2]);	      
		   	  layout.putConstraint(SpringLayout.WEST, radioButtons[3], 10,SpringLayout.WEST, coverPanel);
		   	  layout.putConstraint(SpringLayout.NORTH, labels[3], 30, SpringLayout.SOUTH, labels[2]);	      
		   	  layout.putConstraint(SpringLayout.WEST, labels[3], 20,SpringLayout.WEST, coverPanel);
		   	  
		  /* 		   	  
		   	  layout.putConstraint(SpringLayout.NORTH, radioButtons[6], 10, SpringLayout.SOUTH, l);	      
		   	  layout.putConstraint(SpringLayout.WEST, radioButtons[6], 10,SpringLayout.WEST, contents);
		   	  layout.putConstraint(SpringLayout.NORTH, labels[6], 30, SpringLayout.SOUTH, l);	      
		   	  layout.putConstraint(SpringLayout.WEST, labels[6], 20,SpringLayout.WEST, contents);
		   	  
		   	  layout.putConstraint(SpringLayout.NORTH, radioButtons[7], 10, SpringLayout.SOUTH, labels[6]);	      
		   	  layout.putConstraint(SpringLayout.WEST, radioButtons[7], 10,SpringLayout.WEST, contents);
		   	  layout.putConstraint(SpringLayout.NORTH, labels[7], 30, SpringLayout.SOUTH, labels[6]);	      
		   	  layout.putConstraint(SpringLayout.WEST, labels[7], 20,SpringLayout.WEST, contents);*/
		   	  
		   	  
		      // Buttons
		      JButton okButton = new JButton();
		      okButton.setText("OK");
		      okButton.setActionCommand(CMD_OK);
		      okButton.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      coverPanel.add(okButton);	      
		      Spring con;  
	
		      JButton cancelButton = new JButton();
		      cancelButton.setText("Cancel");
		      cancelButton.setActionCommand(CMD_CANCEL);
		      cancelButton.addActionListener(new ActionListener() { public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      coverPanel.add(cancelButton);
		     
		      JFrame frame=parentFrame;
		      Point pos = frame.getLocationOnScreen();
		      setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);
		      setResizable(false);
		      
		      if(((ParamTableLTLSpecifications)model.getLtlSpecifications().getParamTable()).getLtlModel().getSpecifications().size()==0){
		    	  
		    	  
		    	//  layout.putConstraint(SpringLayout.NORTH, l, 10, SpringLayout.SOUTH, labels[5]);	      
			   	//  layout.putConstraint(SpringLayout.WEST, l, 0,SpringLayout.WEST, contents);
			   	  
		    	  radioButtons[4].setSelected(true);
		    	  radioButtons[0].setVisible(false);
		    	  radioButtons[1].setVisible(false);
		    	  radioButtons[2].setVisible(false);
		    	  radioButtons[3].setVisible(false);
		    	  labels[0].setVisible(false);
		    	  labels[1].setVisible(false);
		    	  labels[2].setVisible(false);
		    	  labels[3].setVisible(false);
		    	//  l.setVisible(true);		    
		    	  layout.putConstraint(SpringLayout.NORTH, radioButtons[4], 90, SpringLayout.NORTH, coverPanel);	      
			   	  layout.putConstraint(SpringLayout.WEST, radioButtons[4], 10,SpringLayout.WEST, coverPanel);
			   	  layout.putConstraint(SpringLayout.NORTH, labels[4], 110, SpringLayout.NORTH, coverPanel);	      
			   	  layout.putConstraint(SpringLayout.WEST, labels[4], 20,SpringLayout.WEST, coverPanel);
		    	  con=layout.getConstraint(SpringLayout.NORTH,labels[3]);
		    	  layout.putConstraint(SpringLayout.NORTH, okButton, Spring.sum(con, Spring.constant(80)),SpringLayout.NORTH, coverPanel);
			      layout.putConstraint(SpringLayout.WEST, okButton, Spring.constant(200),SpringLayout.WEST, coverPanel);
			      layout.putConstraint(SpringLayout.NORTH, cancelButton, Spring.sum(con, Spring.constant(80)), SpringLayout.NORTH, coverPanel);
			      layout.putConstraint(SpringLayout.WEST, cancelButton, 10,SpringLayout.EAST, okButton);
		    	  setSize(new Dimension(500, 300));	 
		    	  
		      }else{
		    	  
		    	//  layout.putConstraint(SpringLayout.NORTH, l, 10, SpringLayout.SOUTH, labels[3]);	      
			   	//  layout.putConstraint(SpringLayout.WEST, l, 0,SpringLayout.WEST, contents);
			   	  
			   	  layout.putConstraint(SpringLayout.NORTH, radioButtons[0], 5, SpringLayout.SOUTH, labels[5]);	      
			   	  layout.putConstraint(SpringLayout.WEST, radioButtons[0], 10,SpringLayout.WEST, coverPanel);
			   	  layout.putConstraint(SpringLayout.NORTH, labels[0],0, SpringLayout.SOUTH, radioButtons[0]);	      
			   	  layout.putConstraint(SpringLayout.WEST, labels[0], 20,SpringLayout.WEST, coverPanel);
		    	  radioButtons[0].setVisible(true);
		    	  radioButtons[1].setVisible(true);
		    	  radioButtons[2].setVisible(true);
		    	  radioButtons[3].setVisible(true);
		    	  labels[0].setVisible(true);
		    	  labels[1].setVisible(true);
		    	  labels[2].setVisible(true);
		    	  labels[3].setVisible(true);
		    	//  l.setVisible(true);
		    	  con=layout.getConstraint(SpringLayout.NORTH,labels[3]);
			      layout.putConstraint(SpringLayout.NORTH, okButton, Spring.sum(con, Spring.constant(60)),SpringLayout.NORTH, coverPanel);
			      layout.putConstraint(SpringLayout.WEST, okButton, Spring.constant(180),SpringLayout.WEST, coverPanel);
			      layout.putConstraint(SpringLayout.NORTH, cancelButton, Spring.sum(con, Spring.constant(60)), SpringLayout.NORTH, coverPanel);
			      layout.putConstraint(SpringLayout.WEST, cancelButton, 10,SpringLayout.EAST, okButton);
			      setSize(new Dimension(500, 500));	
		      }		      
		      setLocationRelativeTo(frame);
		      setVisible(true);	      
	      
	      }else if(n.equals("Check LTL")){
	    	  p=new GradientPanel();
	    	  SpringLayout layout=new SpringLayout();
	    	  p.setLayout(layout);	    
	    	  valueLabel = new JLabel("Simulation duration (optional) ?"); 
	    	  valueText = new JTextField("20");
	    	  //valueText.setColumns(20);
	    	  String txt=model.simulationMap.get("lastSim");//.model.simulationMap.get("numSimDuration");
	    	  if(txt!=null){
	    		  valueText.setText(txt);
	    	  }else{
	    		  valueText.setText("20");
	    	  }	    	
	    	  txt=null;
		      valueText.setColumns(20);		    
		      p.add(valueLabel);		      
		      p.add(valueText);			      
		      layout.putConstraint(SpringLayout.WEST, valueLabel, 50,SpringLayout.WEST, p);
	    	  layout.putConstraint(SpringLayout.NORTH, valueLabel, 30, SpringLayout.NORTH, p);
	    	  layout.putConstraint(SpringLayout.WEST, valueText, 40,SpringLayout.WEST, p);
	    	  layout.putConstraint(SpringLayout.NORTH, valueText, 20,SpringLayout.SOUTH, valueLabel);   	
	    	  
	    	  JButton okButton = new JButton();
		      okButton.setText("Ok");
		      okButton.setActionCommand(CMD_OK);
		      okButton.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      p.add(okButton);
		      layout.putConstraint(SpringLayout.NORTH, okButton, 80, SpringLayout.SOUTH, valueLabel);	
		      JButton cancelButton = new JButton();
		      cancelButton.setText("Cancel");
		      cancelButton.setActionCommand(CMD_CANCEL);
		      cancelButton.addActionListener(new ActionListener() { public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      p.add(cancelButton);
		      layout.putConstraint(SpringLayout.NORTH, cancelButton, 80, SpringLayout.SOUTH, valueLabel);		     
		      layout.putConstraint(SpringLayout.WEST, okButton, 40,SpringLayout.WEST, valueLabel);
		      layout.putConstraint(SpringLayout.WEST, cancelButton, 10,SpringLayout.EAST, okButton);
		    		     
		      coverPanel.setLayout(new BorderLayout());
		      coverPanel.add(p,BorderLayout.CENTER);
		      JFrame frame=parentFrame;
		      Point pos = frame.getLocationOnScreen();
		      setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);
		      setResizable(false);
		      setSize(new Dimension(320, 200));	    
		      setLocationRelativeTo(frame);
		      setVisible(true);	      
	      }else if(n.equals("Compute QFLTL")){
	    	  p=new GradientPanel();
	    	  SpringLayout layout=new SpringLayout();
	    	  p.setLayout(layout);	    
	    	  valueLabel = new JLabel("<html><u>QFLTL Query:</u>"); 
	    	  valueText = new TextFieldLTL(parentFrame,"LTL","QFLTL Query",modelElement);
	    	  valueText.setColumns(20);
	    	  String txt=model.simulationMap.get("QFLTLQuery");
	    	  if(txt!=null){
	    		  valueText.setText(txt);
	    	  }else{
	    		  valueText.setText("Double click to compose");
	    	  }	    	
		      valueText.setColumns(25);		    
		      p.add(valueLabel);		      
		      p.add(valueText);			      
		      layout.putConstraint(SpringLayout.WEST, valueLabel, 10,SpringLayout.WEST, p);
	    	  layout.putConstraint(SpringLayout.NORTH, valueLabel, 30, SpringLayout.NORTH, p);
	    	  layout.putConstraint(SpringLayout.WEST, valueText, 15,SpringLayout.EAST, valueLabel);
	    	  layout.putConstraint(SpringLayout.NORTH, valueText, 30,SpringLayout.NORTH, p);   	
	    	  
	    	  JButton okButton = new JButton();
		      okButton.setText("Check");
		      okButton.setActionCommand(CMD_ANALYZE);
		      okButton.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      p.add(okButton);
		      layout.putConstraint(SpringLayout.NORTH, okButton, 60, SpringLayout.SOUTH, valueLabel);	
		      JButton cancelButton = new JButton();
		      cancelButton.setText("  Cancel  ");
		      cancelButton.setActionCommand(CMD_CANCEL);
		      cancelButton.addActionListener(new ActionListener() { public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      p.add(cancelButton);
		      layout.putConstraint(SpringLayout.NORTH, cancelButton, 60, SpringLayout.SOUTH, valueLabel);		     
		      layout.putConstraint(SpringLayout.WEST, okButton, 100,SpringLayout.WEST, valueLabel);
		      layout.putConstraint(SpringLayout.WEST, cancelButton, 10,SpringLayout.EAST, okButton);
		    		     
		      coverPanel.setLayout(new BorderLayout());
		      coverPanel.add(p,BorderLayout.CENTER);
		      JFrame frame=parentFrame;
		      Point pos = frame.getLocationOnScreen();
		      setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);
		      setResizable(false);
		      setSize(new Dimension(400, 200));	    
		      setLocationRelativeTo(frame);
		      setVisible(true);	      
	      }else if(n.equals("Check LTL Property")){
	    	  
	    	  p=new GradientPanel();
	    	  SpringLayout layout=new SpringLayout();
	    	  p.setLayout(layout);	    
	    	  valueLabel = new JLabel("<html><u>LTL Query:</u>"); 
	    	  valueText = new TextFieldLTL(parentFrame,"LTL","LTL Query",modelElement);
	    	  valueText.setColumns(20);
	    	  String txt=model.simulationMap.get("LTLQuery");
	    	  if(txt!=null){
	    		  valueText.setText(txt);
	    	  }else{
	    		  valueText.setText("Double click to compose");
	    	  }	    	
		      valueText.setColumns(25);		    
		      p.add(valueLabel);		      
		      p.add(valueText);			      
		      layout.putConstraint(SpringLayout.WEST, valueLabel, 10,SpringLayout.WEST, p);
	    	  layout.putConstraint(SpringLayout.NORTH, valueLabel, 30, SpringLayout.NORTH, p);
	    	  layout.putConstraint(SpringLayout.WEST, valueText, 15,SpringLayout.EAST, valueLabel);
	    	  layout.putConstraint(SpringLayout.NORTH, valueText, 30,SpringLayout.NORTH, p);   	
	    	  
	    	  JButton okButton = new JButton();
		      okButton.setText("Check");
		      okButton.setActionCommand(CMD_ANALYZE);
		      okButton.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      p.add(okButton);
		      layout.putConstraint(SpringLayout.NORTH, okButton, 60, SpringLayout.SOUTH, valueLabel);	
		      JButton cancelButton = new JButton();
		      cancelButton.setText("  Cancel  ");
		      cancelButton.setActionCommand(CMD_CANCEL);
		      cancelButton.addActionListener(new ActionListener() { public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      p.add(cancelButton);
		      layout.putConstraint(SpringLayout.NORTH, cancelButton, 60, SpringLayout.SOUTH, valueLabel);		     
		      layout.putConstraint(SpringLayout.WEST, okButton, 100,SpringLayout.WEST, valueLabel);
		      layout.putConstraint(SpringLayout.WEST, cancelButton, 10,SpringLayout.EAST, okButton);
		    		     
		      coverPanel.setLayout(new BorderLayout());
		      coverPanel.add(p,BorderLayout.CENTER);
		      JFrame frame=parentFrame;
		      Point pos = frame.getLocationOnScreen();
		      setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);
		      setResizable(false);
		      setSize(new Dimension(400, 200));	    
		      setLocationRelativeTo(frame);
		      setVisible(true);	      
		      
		      
	      }else if(n.contains("Check CTL Property")){
	    	  
	    	  options=new ArrayList<String>();
	    	  nameLabel = new JLabel("<html><u>Enter a biocham query:<br>or double click to compose</u>");	    	 
	    	  nameText = new TextFieldCTL(parentFrame,modelElement);	
	    	  String txt=model.simulationMap.get("NUSMVQuery");
	    	  if(txt!=null){
	    		  nameText.setText(txt);	    		  
	    	  }else{
	    		  nameText.setText("Double click to compose");
	    	  }
	    	  nameText.setColumns(35);		     
	    	  coverPanel.add(nameLabel);		      
	    	  coverPanel.add(nameText);	
		     
		      layout.putConstraint(SpringLayout.WEST, nameLabel, 10,SpringLayout.WEST, coverPanel);
	    	  layout.putConstraint(SpringLayout.NORTH, nameLabel, 30, SpringLayout.NORTH, coverPanel);
	    	  layout.putConstraint(SpringLayout.WEST, nameText, 10,SpringLayout.EAST, nameLabel);
	    	  layout.putConstraint(SpringLayout.NORTH, nameText, 30,SpringLayout.NORTH, coverPanel);
		       		
	    	  
	    	  JButton okButton = new JButton();
		      okButton.setText("Run");
		      okButton.setActionCommand(CMD_NUSMV);
		      okButton.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      coverPanel.add(okButton);	      		     
		      layout.putConstraint(SpringLayout.NORTH, okButton, 80, SpringLayout.NORTH, nameLabel);
		      layout.putConstraint(SpringLayout.WEST, okButton, 30,SpringLayout.WEST, nameText);
		     
	
		      JButton cancelButton = new JButton();
		      cancelButton.setText("Cancel");
		      cancelButton.setActionCommand(CMD_CANCEL);
		      cancelButton.addActionListener(new ActionListener() { public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      coverPanel.add(cancelButton);
		      layout.putConstraint(SpringLayout.NORTH, cancelButton,  80, SpringLayout.NORTH, nameLabel);
		      layout.putConstraint(SpringLayout.WEST, cancelButton, 10,SpringLayout.EAST, okButton);
		     
		      JFrame frame=parentFrame;//BiochamMainFrame.frame;
		      Point pos = frame.getLocationOnScreen();
		      setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);
		      setResizable(false);
		      setSize(new Dimension(600, 200));	    
		      setLocationRelativeTo(frame);
		      setVisible(true);	      
		      
		      
	      }else if(n.equals("Reduce Model")){
	    	  
	    	  options=new ArrayList<String>();
	    	  nameLabel = new JLabel("Choose rule(s): [optional] ");	  		     
	    	  coverPanel.add(nameLabel);	      
		    
		      
		      ParamTableRules allRules=(ParamTableRules)model.getRules().getParamTable();
		     
		      String rules[]=new String[allRules.getParentRules().size()];
		      for(int h=0;h<rules.length;h++){
		    	  rules[h]=((String) allRules.getParentRules().get(h));
		      }
		      list=new JList(rules);
		      list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		      list.setLayoutOrientation(JList.VERTICAL);
		      list.setVisibleRowCount(1);
		      list.setToolTipText("Tip: Ctrl+Click on the item to deselect it.");		      
		      UniversalScrollPane listScroller = new UniversalScrollPane(list);
		      listScroller.setPreferredSize(new Dimension(530, 220));
		      coverPanel.add(listScroller);
		      
		      layout.putConstraint(SpringLayout.WEST, nameLabel, 200,SpringLayout.WEST, coverPanel);
	    	  layout.putConstraint(SpringLayout.NORTH, nameLabel, 25, SpringLayout.NORTH, coverPanel);
	    	  layout.putConstraint(SpringLayout.WEST, listScroller, 30,SpringLayout.WEST, coverPanel);
	    	  layout.putConstraint(SpringLayout.NORTH, listScroller, 60, SpringLayout.NORTH, coverPanel);
	    	 		    
		       		
	    	  
	    	  JButton okButton = new JButton();
		      okButton.setText("Reduce");
		      okButton.setActionCommand(CMD_OK);
		      okButton.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      coverPanel.add(okButton);	      		     
		      layout.putConstraint(SpringLayout.NORTH, okButton, 320, SpringLayout.NORTH, coverPanel);
		      layout.putConstraint(SpringLayout.WEST, okButton, 400,SpringLayout.WEST, coverPanel);
		     
	
		      JButton cancelButton = new JButton();
		      cancelButton.setText("Cancel");
		      cancelButton.setActionCommand(CMD_CANCEL);
		      cancelButton.addActionListener(new ActionListener() { public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      coverPanel.add(cancelButton);
		      layout.putConstraint(SpringLayout.NORTH, cancelButton, 320, SpringLayout.NORTH, coverPanel);
		      layout.putConstraint(SpringLayout.WEST, cancelButton, 10,SpringLayout.EAST, okButton);
		     
		      JFrame frame=parentFrame;
		      Point pos = frame.getLocationOnScreen();
		      setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);
		      setResizable(false);
		      setSize(new Dimension(600, 400));	    
		      setLocationRelativeTo(frame);
		      setVisible(true);	      
		      
		      
	      }else if(n.equals("Revise Model")){
	    	  
	    	  nameLabel = new JLabel("Choose rule(s): [optional] ");	  		     
	    	  coverPanel.add(nameLabel);	      
		    
		      ParamTableRules allRules=(ParamTableRules)model.getRules().getParamTable();
		      String rules[]=new String[allRules.getParentRules().size()];
		      for(int h=0;h<rules.length;h++){
		    	  rules[h]=((String) allRules.getParentRules().get(h));
		      }
		      list=new JList(rules);
		      list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		      list.setLayoutOrientation(JList.VERTICAL);
		      list.setVisibleRowCount(1);
		      list.setToolTipText("Tip: Ctrl+Click on the item to deselect it.");
		     
		      UniversalScrollPane listScroller = new UniversalScrollPane(list);
		      listScroller.setPreferredSize(new Dimension(530, 220));
		      coverPanel.add(listScroller);
		      
		      reviseOption=new JCheckBox("Interactive");
		      reviseOption.setOpaque(false);
		      reviseOption.setSelected(false);
		      coverPanel.add(reviseOption);
		      
		      layout.putConstraint(SpringLayout.WEST, nameLabel, 200,SpringLayout.WEST, coverPanel);
	    	  layout.putConstraint(SpringLayout.NORTH, nameLabel, 25, SpringLayout.NORTH, coverPanel);
	    	  layout.putConstraint(SpringLayout.WEST, listScroller, 30,SpringLayout.WEST, coverPanel);
	    	  layout.putConstraint(SpringLayout.NORTH, listScroller, 60, SpringLayout.NORTH, coverPanel);
	    	  layout.putConstraint(SpringLayout.WEST, reviseOption, 30,SpringLayout.WEST, coverPanel);
	    	  layout.putConstraint(SpringLayout.NORTH, reviseOption, 290, SpringLayout.NORTH, coverPanel);
	    	 		    
		       		
	    	  
	    	  JButton okButton = new JButton();
		      okButton.setText("Revise");
		      okButton.setActionCommand(CMD_OK);
		      okButton.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      coverPanel.add(okButton);	      		     
		      layout.putConstraint(SpringLayout.NORTH, okButton, 320, SpringLayout.NORTH, coverPanel);
		      layout.putConstraint(SpringLayout.WEST, okButton, 400,SpringLayout.WEST, coverPanel);
		     
	
		      JButton cancelButton = new JButton();
		      cancelButton.setText("Cancel");
		      cancelButton.setActionCommand(CMD_CANCEL);
		      cancelButton.addActionListener(new ActionListener() { public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      coverPanel.add(cancelButton);
		      layout.putConstraint(SpringLayout.NORTH, cancelButton, 320, SpringLayout.NORTH, coverPanel);
		      layout.putConstraint(SpringLayout.WEST, cancelButton, 10,SpringLayout.EAST, okButton);
		     
		      JFrame frame=parentFrame;
		      Point pos = frame.getLocationOnScreen();
		      setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);
		      setResizable(false);
		      setSize(new Dimension(600, 400));	    
		      setLocationRelativeTo(frame);
		      setVisible(true);	      
		      
		      
	      }else if(n.equals("Learn Rules")){
	    	  
	    	  nameLabel = new JLabel("Choose rule(s):");	  		     
	    	  coverPanel.add(nameLabel);	      
		    
		      nameLabel2 = new JLabel("<html><u>Rule or set of rules:</u>");	    	 
	    	  nameText = new TextFieldRules(model);
	    	  String txt=model.simulationMap.get("learnRules");
	    	  if(txt!=null){
	    		  nameText.setText(txt);
	    	  }else{
	    		  nameText.setText("Double click to compose");
	    	  }
	    	  nameText.setColumns(35);		     
	    	  coverPanel.add(nameLabel2);		      
	    	  coverPanel.add(nameText);		
	    	  	
	    	  ParamTableRules allRules=(ParamTableRules)model.getRules().getParamTable();
		      String rules[]=new String[allRules.getParentRules().size()];
		      for(int h=0;h<rules.length;h++){
		    	  rules[h]=((String) allRules.getParentRules().get(h));
		      }
		      list=new JList(rules);
		      list.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
		      list.setLayoutOrientation(JList.VERTICAL);
		      list.setVisibleRowCount(1);
		      list.setToolTipText("Tip: Ctrl+Click on the item to deselect it.");
		      
		      listScroller = new UniversalScrollPane(list);
		      listScroller.setPreferredSize(new Dimension(530, 200));
		      
		      addition=new JRadioButton("Addition");
		      addition.setOpaque(false);
		      addition.setSelected(true);
		      addition.addActionListener(new ActionListener() {
		            public void actionPerformed(ActionEvent e) {
		                JRadioButton rb = (JRadioButton)e.getSource();
		                add = rb.isSelected();
		                if(!add){
		                	Component[] comps=coverPanel.getComponents();
		                	for(int i=0;i<comps.length;i++){
		                		if(comps[i].equals(nameLabel2)){
		                			coverPanel.remove(nameLabel2);
		                		}else if(comps[i].equals(nameText)){
		                			coverPanel.remove(nameText);
		                		}
		                	}                    	
		                	coverPanel.add(listScroller); 
		  		    	  	layout.putConstraint(SpringLayout.WEST, listScroller, 10,SpringLayout.WEST, coverPanel);
		  		    	  	layout.putConstraint(SpringLayout.NORTH, listScroller, 80, SpringLayout.NORTH, coverPanel);
		  		    	  	coverPanel.validate();
		  		    	  	coverPanel.repaint();
		  		      	}else{
		  		      		
			  		      	Component[] comps=coverPanel.getComponents();
		                	for(int i=0;i<comps.length;i++){
		                		if(comps[i].equals(listScroller)){
		                			coverPanel.remove(listScroller);
		                		}
		                	}
		                	coverPanel.add(nameLabel2);		      
		                	coverPanel.add(nameText);		
		  		    	  	layout.putConstraint(SpringLayout.WEST, nameLabel2, 10,SpringLayout.WEST, coverPanel);
		  		    	  	layout.putConstraint(SpringLayout.NORTH, nameLabel2, 80, SpringLayout.NORTH, coverPanel);
		  		    	  	layout.putConstraint(SpringLayout.WEST, nameText, 10,SpringLayout.EAST, nameLabel2);
		  		    	  	layout.putConstraint(SpringLayout.NORTH, nameText, 80,SpringLayout.NORTH, coverPanel);
		  		    	    coverPanel.validate();
		  		    	    coverPanel.repaint();
		  		      	}
		            }
		      });
		      list.setSelectedIndex(0);
		      deletion=new JRadioButton("Deletion");
		      deletion.setOpaque(false);
		      deletion.setSelected(false);
		      deletion.addActionListener(new ActionListener() {
		            public void actionPerformed(ActionEvent e) {
		                JRadioButton rb = (JRadioButton)e.getSource();
		                delete = rb.isSelected();
		                if(delete){
		                	Component[] comps=coverPanel.getComponents();
		                	for(int i=0;i<comps.length;i++){
		                		if(comps[i].equals(nameLabel2)){
		                			coverPanel.remove(nameLabel2);
		                		}else if(comps[i].equals(nameText)){
		                			coverPanel.remove(nameText);
		                		}
		                	}                    	
		                	coverPanel.add(listScroller); 
		  		    	  	layout.putConstraint(SpringLayout.WEST, listScroller, 10,SpringLayout.WEST, coverPanel);
		  		    	  	layout.putConstraint(SpringLayout.NORTH, listScroller, 80, SpringLayout.NORTH, coverPanel);
		  		    	  	//contents.validate();
		  		    	    coverPanel.repaint();  
			  		  	}else{
			  		  		Component[] comps=coverPanel.getComponents();
			  		  		for(int i=0;i<comps.length;i++){
			  		  			if(comps[i].equals(listScroller)){
			  		  			coverPanel.remove(listScroller);
			  		  			}
			  		  		}
			  		  		coverPanel.add(nameLabel2);		      
			  		  		coverPanel.add(nameText);
			  		  		layout.putConstraint(SpringLayout.WEST, nameLabel2, 10,SpringLayout.WEST, coverPanel);
			  		  		layout.putConstraint(SpringLayout.NORTH, nameLabel2, 80, SpringLayout.NORTH, coverPanel);
			  		  		layout.putConstraint(SpringLayout.WEST, nameText, 10,SpringLayout.EAST, nameLabel2);
			  		  		layout.putConstraint(SpringLayout.NORTH, nameText, 80,SpringLayout.NORTH, coverPanel);
			  		  		//contents.validate();
			  		  	    coverPanel.repaint();
			  		  	}
		            }
		      });
		      group.add(addition);
		      group.add(deletion);
		      coverPanel.add(addition);
		      coverPanel.add(deletion);
		      
		      layout.putConstraint(SpringLayout.WEST, addition, 5,SpringLayout.WEST, coverPanel);
	    	  layout.putConstraint(SpringLayout.NORTH, addition, 15, SpringLayout.NORTH, coverPanel);
	    	  layout.putConstraint(SpringLayout.WEST, deletion, 60,SpringLayout.EAST, addition);
	    	  layout.putConstraint(SpringLayout.NORTH, deletion, 15, SpringLayout.NORTH, coverPanel);	    	  
		      layout.putConstraint(SpringLayout.WEST, nameLabel, 10,SpringLayout.WEST, coverPanel);
	    	  layout.putConstraint(SpringLayout.NORTH, nameLabel, 50, SpringLayout.NORTH, coverPanel);
	    	  layout.putConstraint(SpringLayout.WEST, nameLabel2, 10,SpringLayout.WEST, coverPanel);
	    	  layout.putConstraint(SpringLayout.NORTH, nameLabel2, 80, SpringLayout.NORTH, coverPanel);
	    	  layout.putConstraint(SpringLayout.WEST, nameText, 10,SpringLayout.EAST, nameLabel2);
	    	  layout.putConstraint(SpringLayout.NORTH, nameText, 80,SpringLayout.NORTH, coverPanel);
	    	  
	    	  JButton okButton = new JButton();
		      okButton.setText("Learn");
		      okButton.setActionCommand(CMD_OK);
		      okButton.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      coverPanel.add(okButton);	      		     
		      layout.putConstraint(SpringLayout.NORTH, okButton, 320, SpringLayout.NORTH, coverPanel);
		      layout.putConstraint(SpringLayout.WEST, okButton, 400,SpringLayout.WEST, coverPanel);
		     
	
		      JButton cancelButton = new JButton();
		      cancelButton.setText("Cancel");
		      cancelButton.setActionCommand(CMD_CANCEL);
		      cancelButton.addActionListener(new ActionListener() { public void actionPerformed(ActionEvent event) { windowAction(event);}});
		      coverPanel.add(cancelButton);
		      layout.putConstraint(SpringLayout.NORTH, cancelButton, 320, SpringLayout.NORTH, coverPanel);
		      layout.putConstraint(SpringLayout.WEST, cancelButton, 10,SpringLayout.EAST, okButton);
		     
		      JFrame frame=parentFrame;
		      Point pos = frame.getLocationOnScreen();
		      setLocation(pos.x+frame.getSize().width/2-185,pos.y+frame.getSize().height/2-140);
		      setResizable(false);
		      setSize(new Dimension(600, 400));	    
		      setLocationRelativeTo(frame);
		      setVisible(true);	      
		      
		      
	      }


	   }

	   public String getNewOneParameter(){
		   
		   return getName();
	   }
	   
	   public Object[] getNewParameter(){
		   		   	
		   Object[] parameter=new Object[2];
		   String name=getName();
		   
		   if(dialogName.contains("Check CTL Property")){
			   
			  
			   parameter[0]=name;
			   boolean reordering=((TopLeftCTLPanelFirst)panel).reorderingOption.isSelected();//((TopLeftPanel)panel).//.getComponent(3).issparent.((ParamTableCTLSpecifications)model.getCtlSpecifications().getParamTable()).getCtlModel().getReorderingOption().isSelected();
			   boolean fairness=((TopLeftCTLPanelFirst)panel).fairnessOption.isSelected();
			   boolean mode=((TopLeftCTLPanelFirst)panel).modeOption.isSelected();
			   boolean why=((TopLeftCTLPanelFirst)panel).whyOption.isSelected();
			   if(reordering){
				   options.add(0,"nusmv_dynamic_reordering.\n");
			   }else{
				   options.add(0,"nusmv_disable_dynamic_reordering.\n");
			   } 
			   if(fairness){
				   options.add(1,"fairness_path.\n");
			   }else{
				   options.add(1,"no_fairness_path.\n");
			   } 
			   if(mode){
				   options.add(2,"nusmv_direct.\n");
			   }else{
				   options.add(2,"nusmv_non_direct.\n");
			   }
			   if(why){
				   options.add(3,"check_why");
			   }else {
				   options.add(3,"check_ctl");
			   }
			   parameter[1]=options;
		   }else if(dialogName.equals("Reduce Model") || dialogName.equals("Revise Model")){			   
			   parameter[0]=list.getSelectedValues();
			   parameter[1]=name;
			   
		   }else if(dialogName.equals("Learn Rules")){
			   if(name!=null){
				   if(name.equals("addition")){
					   parameter[1]=name;
					   parameter[0]=getValue();
				   }else{
					   parameter[0]=list.getSelectedValues();
					   parameter[1]=name;
				   }
			   }
			   
		   }else{
			   name=getName();
			   parameter[0]=name;
			   parameter[1]=getValue();
		   }
		   return parameter;
		  
	   } 
	   
	   
	   
	  
	   
	   /**
	    * The user has selected an option.
	    * If actionCommand is an ActionEvent, getCommandString() is called,
	    * otherwise toString() is used to get the action command.
	    *
	    * @param actionCommand may be null
	    */
	   private void windowAction(Object actionCommand) {
	      boolean closeWindow = false;
	      String cmd = null;
	      if (actionCommand != null) {
	         if (actionCommand instanceof ActionEvent) {
	            cmd = ((ActionEvent)actionCommand).getActionCommand();
	         } else {
	            cmd = actionCommand.toString();
	         }
	      }
	      if (cmd == null) {
	         // do nothing
	      }else if(cmd.equals("NumSim")){
	    	  String method = null;
	    	  int time=20;
	    	  for(int i=0;i<4;i++){
	    		  if(radioButtons[i].isSelected()){
	    			  model.simulationMap.put("numMethod", String.valueOf(i));
	    			  method=radioButtons[i].getActionCommand();
	    		  }
	    	  }
	    	  String bcCommand="numerical_method("+method+").\n";	    	 
	    	  if(stepsText.getText()!=null){
	    		  float f=Float.parseFloat(stepsText.getText());
	    		  model.simulationMap.put("numSteps",stepsText.getText());
	    		  bcCommand+="step_size("+f+").\n";
	    	  }
	    	  if(timeText.getText()!=null){
	    		  int f=Integer.parseInt(timeText.getText());
	    		  model.simulationMap.put("lastSim",timeText.getText());	
	    		  time=f;
	    	  }
	    	  if(stepsDoubling.getText()!=null){
	    		  float f=Float.parseFloat(stepsDoubling.getText());
	    		  model.simulationMap.put("stepsDoubling",stepsDoubling.getText());
	    		  bcCommand+="step_doubling("+f+").\n";
	    	  }else{
	    		  bcCommand+="no_step_doubling.\n";
	    	  }
	    	  String cf=convFText.getText();
	    	  if(convFText.getText()!=null && convFText.getText().length()>0){
	    		  int f=Integer.parseInt(convFText.getText());
	    		  model.simulationMap.put("convFact",convFText.getText());
	    		  bcCommand+="conversion_factor("+f+").\n";
	    	  }
	    	  String th=threshold.getText();
	    	  if(threshold.getText()!=null && threshold.getText().length()>0){
	    		  int f=Integer.parseInt(threshold.getText());
	    		  model.simulationMap.put("threshold",threshold.getText());
	    		  bcCommand+="critical_reaction_threshold("+f+").\n";
	    	  }
	    	  String s=xminT.getText();
	    	  if(s!=null && s!="" && s.length()>0 && !s.equals(null)){
	    		  float f=Float.parseFloat(s);
	    		  model.simulationMap.put("xmin",s);
	    		  bcCommand+="set_xmin("+f+").\n";
	    	  }
	    	  s=xmaxT.getText();
	    	  if(s!=null && s!="" && s.length()>0 && !s.equals(null)){
	    		  float f=Float.parseFloat(s);
	    		  model.simulationMap.put("xmax",s);
	    		  bcCommand+="set_xmax("+f+").\n";
	    	  }
	    	  s=yminT.getText();
	    	  if(s!=null && s!="" && s.length()>0 && !s.equals(null)){
	    		  float f=Float.parseFloat(s);
	    		  model.simulationMap.put("ymin",s);
	    		  bcCommand+="set_ymin("+f+").\n";
	    	  }
	    	  s=ymaxT.getText();
	    	  if(s!=null && s!="" && s.length()>0 && !s.equals(null)){
	    		  float f=Float.parseFloat(s);
	    		  model.simulationMap.put("ymax",s);
	    		  bcCommand+="set_ymax("+f+").\n";
	    	  }
	    	  
	    	  Object[] molecules=((JList)listScroller.getViewport().getView()).getSelectedValues();
	    	  Object[] macros=((JList)listScroller1.getViewport().getView()).getSelectedValues();
	    	  int len1=molecules.length;
	    	  ArrayList<String> selected=new ArrayList<String>();
	    	  for(int i=0;i<len1;i++){
	    		  selected.add((String)molecules[i]);
	    	  }
	    	  ArrayList<String> all=new ArrayList<String>();
	    	  
	    	  MoleculesModel molsModel=((ParamTableMolecules)model.getMolecules().getParamTable()).getMoleculesModel();
	    	  int molSize=molsModel.getMolecules().size();	     
	    	
	    	  for(int h=0;h<molSize;h++){
	    		  all.add(molsModel.getMolecules().get(h));
	    	  }
	    	 
	    	  bcCommand+="show_molecules({";
	    	  for(int i=0;i<all.size();i++){
	    		  bcCommand+=all.get(i);
	    		  if(i<all.size()-1){
	    			  bcCommand+=",";
	    		  }
	    	  }
	    	  bcCommand+="}).\n";
	    	  
	    	  all.removeAll(selected);
	    	  
	    	  
	    	  if(selected.size()>0 && all.size()>0){
		    	  bcCommand+="hide_molecules({";
		    	  for(int i=0;i<all.size();i++){
		    		  bcCommand+=all.get(i);
		    		  if(i<all.size()-1){
		    			  bcCommand+=",";
		    		  }
		    	  }
		    	  bcCommand+="}).\n";
	    	  }else if(all.size()>0 && selected.size()==0){
	    		  
	    		  bcCommand+="show_molecules({";
		    	  for(int i=0;i<all.size();i++){
		    		  bcCommand+=all.get(i);
		    		  if(i<all.size()-1){
		    			  bcCommand+=",";
		    		  }
		    	  }
		    	  bcCommand+="}).\n";
	    	  }
	    	  
	    	  
	    	  int len2=macros.length;//macros to be shown...
	    	 
	    	  if(len2>0){    	   	 	  
		    	  bcCommand+="show_macros({";
		    	  for(int i=0;i<len2;i++){
		    		  bcCommand+=(String)macros[i];
		    		  if(i<len2-1){
		    			  bcCommand+=",";
		    		  }
		    	  }
		    	  bcCommand+="}).\n";   	  
	    	  }
	    	  if(maxT){
	    		  for(int i=0;i<selected.size();i++){
	    			  bcCommand+="get_max_from_trace("+selected.get(i)+").\n";
	    		  }
	    	  }
	    	  if(minT){
	    		  for(int i=0;i<selected.size();i++){
	    			  bcCommand+="get_min_from_trace("+selected.get(i)+").\n";
	    		  }
	    	  }
	    	  
	    	  setName(bcCommand);
	    	  setValue(timeText.getText());
	    	  closeWindow = true;
	    	  selected.clear();all.clear();
	    	  selected=null;all=null;
	    	 
	      } 
	      else if (cmd.equals(CMD_CANCEL)) {
	    	  
	    	  setName(null);	
    		  setValue(null);
	    	  closeWindow = true;
	    	  
	      } else if (cmd.equals(CMD_OK)) {
	    	  if(dialogName.equals("Reduce Model")){	    		
  	             setName("nothing");
  	             closeWindow = true;
	    	  }else if(dialogName.equals("Learn Rules")){
  	              if(addition.isSelected()){
  	            	  setName("addition");
  	            	  String txt=nameText.getText();
  	            	  setValue(txt);
  	            	  model.simulationMap.put("learnRules", txt);
  	              }else{
  	            	  setName("deletion");
  	            	  
  	              }
  	            closeWindow = true;
	    	  }else if(dialogName.equals("Revise Model")){
	    		  
  	              if(reviseOption.isSelected()){
  	            	  setName("selected");
  	              }else{
  	            	  setName("nothing");
  	              }
  	            closeWindow = true;
	    	  }else if(dialogName.equals("Check LTL")){
	    		  name=valueText.getText().trim();
	    		  if(name!=null && name!="" && !name.startsWith("0") && !name.startsWith("0.0")){
	    			  setName(name);
	    			  setValue(name);
			    	  model.simulationMap.put("numSimDuration", name);  
	    		  }else{
	    			  setName(null);   			  
	    			  setValue("20");
	    		  }
	    		  
		    	  closeWindow = true;
	    	  }else if(dialogName.equals("Check LTL Property")){
	    		  name=valueText.getText().trim();
	    		  if(name!=null && name!=""){
	    			  setName(name);
	    			  setValue(name);
			    	  model.simulationMap.put("LTLQuery", name);  
	    		  }else{
	    			  setName(null);   			  
	    			  setValue(null);
	    		  }
	    		  
		    	  closeWindow = true;
	    	  }else if(dialogName.equals("Compute QFLTL")){
	    		  name=valueText.getText().trim();
	    		  if(name!=null && name!=""){
	    			  setName(name);
	    			  setValue(name);
			    	  model.simulationMap.put("QFLTLQuery", name);  
	    		  }else{
	    			  setName(null);   			  
	    			  setValue(null);
	    		  }
	    		  
		    	  closeWindow = true;
	    	  }
	    	  else{
		    	  try {	
		    		  for(int k=0;k<6;k++){
		    			  if(radioButtons[k].isSelected()){
		    				  setName(radioButtons[k].getActionCommand());
		    			  }
		    		  }
		    		
		             closeWindow = true;
		            
		         } catch  (Exception excp) {
		        	 
		        	 JOptionPane.showMessageDialog(this,"Invalid value:","Invalid value",JOptionPane.WARNING_MESSAGE);
		         }
	    	  }
	         
	      }else if(cmd.equals(CMD_ANALYZE)){	    	  
	    	  
	    	  //setName(nameText.getText());
	    	  setValue(valueText.getText());
	    	  model.simulationMap.put("traceQuery",valueText.getText());
	    	  closeWindow = true;
	    	  
	      }else if(cmd.equals(CMD_NUSMV)){
	    	  String name=nameText.getText();
	    	  if(options==null){
	    		  options=new ArrayList();
	    	  }
			  
	    	  boolean reordering=((TopLeftCTLPanelFirst)panel).reorderingOption.isSelected();//((TopLeftPanel)panel).//.getComponent(3).issparent.((ParamTableCTLSpecifications)model.getCtlSpecifications().getParamTable()).getCtlModel().getReorderingOption().isSelected();
	    	  boolean fairness=((TopLeftCTLPanelFirst)panel).fairnessOption.isSelected();
	    	  boolean mode=((TopLeftCTLPanelFirst)panel).modeOption.isSelected();
	    	  boolean why=((TopLeftCTLPanelFirst)panel).whyOption.isSelected();
	    	  if(reordering){
	    		  options.add(0,"nusmv_dynamic_reordering.\n");
	    	  }else{
	    		  options.add(0,"nusmv_disable_dynamic_reordering.\n");
	    	  } 
	    	  if(fairness){
	    		  options.add(1,"fairness_path.\n");
	    	  }else{
	    		  options.add(1,"no_fairness_path.\n");
	    	  } 
	    	  if(mode){
	    		  options.add(2,"nusmv_direct.\n");
	    	  }else{
	    		  options.add(2,"nusmv_non_direct.\n");
	    	  }
	    	  if(why){
	    		  options.add(3,"check_why");
	    	  }else {
	    		  options.add(3,"check_ctl");
	    	  }
	    	  setName(name);
	    	  model.simulationMap.put("NUSMVQuery", name);
	    	  closeWindow = true;
	      }
	      else{}      
	      
	      if (closeWindow) {
	         setVisible(false);
	         dispose();
	      }
	      
	   }
	   
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}

	public void disposeObject() {
		if(options!=null){
			options.clear();
			options=null;
		}
		
	}

	public void actionPerformed(ActionEvent e) {
		if(radioButtons[0].isSelected() || radioButtons[1].isSelected()){
			threshold.setText("");
			threshold.setEditable(false);
			convFText.setText("");
			convFText.setEditable(false);			
		}else{
			threshold.setText("20");
			threshold.setEditable(true);
			convFText.setEditable(true);
			convFText.setText("10000");
		}
		
	}

	
	   
	   
	   
	
}


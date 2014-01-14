package fr.inria.contraintes.biocham.dialogs;

import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.customComponents.UniversalScrollPane;
import fr.inria.contraintes.biocham.modelData.MacrosModel;
import fr.inria.contraintes.biocham.modelData.MoleculesModel;
import fr.inria.contraintes.biocham.modelData.ParamTableMacros;
import fr.inria.contraintes.biocham.modelData.ParamTableMolecules;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSeparator;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.Spring;
import javax.swing.SpringLayout;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

public class DialogSimulation extends JFrame{

	
	BiochamModel model;
	SpringLayout layout, layout2;
	Spring maxSpring;
	Container contents;
	JPanel intro, main, buttons;
	JPanel ode,stoch,bool;
	JPanel shows,options;
	JCheckBox odeCB,stochCB,boolCB;
	ButtonGroup group1, group2;
	JRadioButton rosen,rk,tp,gille;
	UniversalScrollPane listScroller,listScroller1;
	JTextField timeText,stepsText,convFText,threshold, stepsDoubling;	  
	JList macroList;
	JCheckBox steps_doubling;

	
	
	public DialogSimulation(BiochamModel m){
				
		super("Simulation");
		model=m;
		createGUI();
		Point pos = BiochamMainFrame.frame.getLocationOnScreen();
	    setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
	    setResizable(true);
	    setSize(new Dimension(480, 580));	    
	    setLocationRelativeTo(BiochamMainFrame.frame);
	    setVisible(true);	    
	   
	    
	}
	
	
	
	
	private void createGUI() {
		
		setIconImage(Icons.images.get("chart_curve.png"));
		contents= getContentPane();
		contents.setLayout(new BorderLayout());
	    createMainPanel();
	    createButtonsPanel();
	    
	    
	}




	/**
	 * 
	 */
	private void createMainPanel() {
	
		SpringLayout lay=new SpringLayout();
		main=new JPanel(lay);		
	    main.setBackground(Utils.backgroundColor);	   		
		odeCB=new JCheckBox("ODE");
		odeCB.setBackground(Utils.backgroundColor);
		odeCB.setSelected(true);		
		odeCB.addChangeListener(new ChangeListener(){

			public void stateChanged(ChangeEvent e) {
				if(odeCB.isSelected()){
				
					boolCB.setSelected(false);
					stochCB.setSelected(false);
					gille.setEnabled(false);
					tp.setEnabled(false);
					rk.setEnabled(true);
					rosen.setEnabled(true);
					macroList.setEnabled(true);
					stepsText.setEnabled(true);
					stepsDoubling.setEnabled(true);
					steps_doubling.setEnabled(true);
					convFText.setEnabled(false);
					threshold.setEnabled(false);
				}
				
			}});
		stochCB=new JCheckBox("Stochastic");
		stochCB.setBackground(Utils.backgroundColor);
		stochCB.setSelected(false);
		stochCB.addChangeListener(new ChangeListener(){

			public void stateChanged(ChangeEvent e) {
				if(stochCB.isSelected()){
					
					boolCB.setSelected(false);
					odeCB.setSelected(false);
					rosen.setEnabled(false);
					rk.setEnabled(false);
					gille.setEnabled(true);
					tp.setEnabled(true);
					macroList.setEnabled(true);
					stepsText.setEnabled(false);
					stepsDoubling.setEnabled(false);
					steps_doubling.setEnabled(false);
					convFText.setEnabled(true);
					convFText.setEditable(true);
					threshold.setEnabled(true);
					threshold.setEditable(true);
				}
				
			}});
		boolCB=new JCheckBox("Boolean");
		boolCB.setBackground(Utils.backgroundColor);
		boolCB.setSelected(false);
		boolCB.addChangeListener(new ChangeListener(){

			public void stateChanged(ChangeEvent e) {
				if(boolCB.isSelected()){
				
					odeCB.setSelected(false);
					stochCB.setSelected(false);
					rosen.setEnabled(false);
					rk.setEnabled(false);
					tp.setEnabled(false);
					gille.setEnabled(false);
					macroList.setEnabled(false);
					stepsText.setEnabled(false);
					stepsDoubling.setEnabled(false);
					steps_doubling.setEnabled(false);
					convFText.setEnabled(false);
					threshold.setEnabled(false);
					
				}
				
			}});
		
		rosen=new JRadioButton("Rosenbrock");
		rosen.setBackground(Utils.backgroundColor);
		rosen.setActionCommand("rosen");
		rosen.setSelected(true);
		rk=new JRadioButton("Runge-Kutta");
		rk.setBackground(Utils.backgroundColor);
		rk.setActionCommand("rk");
		rk.setSelected(false);
		tp=new JRadioButton("Tau-Leaping");
		tp.setBackground(Utils.backgroundColor);
		tp.setActionCommand("tp");
		tp.setSelected(false);
		gille=new JRadioButton("Gillespie");
		gille.setActionCommand("gille");
		gille.setBackground(Utils.backgroundColor);
		gille.setSelected(false);
/*		rosen.addActionListener(this);
		rk.addActionListener(this);
		tp.addActionListener(this);
		gille.addActionListener(this);*/
		
		group1 = new ButtonGroup();
		group2 = new ButtonGroup();		
   	    group1.add(rosen);		
   	    group1.add(rk);
   	    group2.add(tp);
   	    group2.add(gille);
   	    
   	    main.add(odeCB);
   	    main.add(rosen);
   	    main.add(rk);
   	    main.add(stochCB);
	    main.add(tp);
	    main.add(gille);
	    main.add(boolCB); 
	    
	   
   	    
   	    
	    JLabel showMols=new JLabel("<html><u>Show molecules:</u>");     
	    JLabel showMacros=new JLabel("<html><u>Show macros:</u>");
	    	    
	    MoleculesModel molsModel=((ParamTableMolecules)model.getMolecules().getParamTable()).getMoleculesModel();
		int size=molsModel.getMolecules().size();	     
	    String molecules[]=new String[size];
	    for(int h=0;h<size;h++){
	    	molecules[h]=molsModel.getMolecules().get(h);
	    }
	    JList molList=new JList(molecules);
	    molList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
	    molList.setLayoutOrientation(JList.VERTICAL);
	    molList.setVisibleRowCount(1);
	    molList.setToolTipText("Tip: Ctrl+Click on the item to deselect it.");		      
	    listScroller = new UniversalScrollPane(molList);
	    listScroller.setPreferredSize(new Dimension(200, 100));
	    
	    MacrosModel macrosModel=((ParamTableMacros)model.getMacros().getParamTable()).getMacrosModel();
	    int msize=macrosModel.getMacros().size();	     
	    String macros[]=new String[msize];
	    Iterator it = macrosModel.getMacros().entrySet().iterator();
	    int i=0;
	    //******************HERE STOP*************************//
	    while(it.hasNext()){
	      Map.Entry<String,String> me = (Map.Entry)it.next();	     
	      macros[i]=macrosModel.getMacros().get(me.getKey());
	      i++;
	    }
	   
	    
	   
	    macroList=new JList(macros);
	    macroList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
	    macroList.setLayoutOrientation(JList.VERTICAL);
	    macroList.setVisibleRowCount(1);
	    macroList.setToolTipText("Tip: Ctrl+Click on the item to deselect it.");		      
	    listScroller1 = new UniversalScrollPane(macroList);
	    listScroller1.setPreferredSize(new Dimension(200, 100));
	   	   	
	    main.add(showMols);
	    main.add(showMacros);
	    main.add(listScroller);
	    main.add(listScroller1);
	   	    
	   	    
	    JLabel time=new JLabel("Time/Transitions:");
	    main.add(time);		      
	    timeText=new JTextField(4);
	    String txt=model.simulationMap.get("lastSim");
	    if(txt!=null){
	      timeText.setText(txt);
	    }else{
	      timeText.setText("200");
	    }
	    main.add(timeText);
	    
	    JLabel steps=new JLabel("Step size:");
	    main.add(steps);
	    stepsText=new JTextField(4);
	    txt=model.simulationMap.get("numSteps");
	    if(txt!=null){
	    	stepsText.setText(txt);
	    }else{
	    	stepsText.setText("0.01");
	    }
	    main.add(stepsText);
	      
	    steps_doubling=new JCheckBox("Steps Doubling:");
	    steps_doubling.setSelected(false);
	    steps_doubling.setBackground(Utils.backgroundColor);
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
	    main.add(steps_doubling);
	    stepsDoubling=new JTextField(4);
	    txt=model.simulationMap.get("stepsDoubling");
	    if(txt!=null){
	    	stepsDoubling.setText(txt);
	    }else{
	    	stepsDoubling.setText("0.0001");
	    }
	    stepsDoubling.setEditable(false);
	    main.add(stepsDoubling);
	      
	    JLabel convFactor=new JLabel("Conversion Factor:");
	    main.add(convFactor);
	    convFText=new JTextField(5);
	    convFText.setText("10");
	    if(rk.isSelected() || rosen.isSelected()){
	    	  txt=model.simulationMap.get("convFact");
		      if(txt!=null){
		    	  convFText.setText(txt);
		      }else{
		    	  convFText.setText("10");
		      }
	    	  convFText.setEditable(false);
	      }else{
	    	  convFText.setEditable(true);
	      }
	    main.add(convFText);
	    JLabel crThreshold=new JLabel("Critical Reaction Threshold:");
	    main.add(crThreshold);
	    threshold=new JTextField(4); 
	    threshold.setText("20");
	    if(rk.isSelected() || rosen.isSelected()){
	    	txt=model.simulationMap.get("threshold");
		      if(txt!=null){
		    	  threshold.setText(txt);
		      }else{
		    	  threshold.setText("20");
		      }	    	  
	    	  threshold.setEditable(false);	
	      }else{
	    	  threshold.setEditable(true);
	    	  
	      }
	   
	    main.add(threshold);
	      
	    
	    
	    lay.putConstraint(SpringLayout.WEST, odeCB, 15, SpringLayout.WEST,main);
   	    lay.putConstraint(SpringLayout.NORTH, odeCB, 15, SpringLayout.NORTH,main);
   	    lay.putConstraint(SpringLayout.WEST, stochCB, 110, SpringLayout.EAST,odeCB);
	    lay.putConstraint(SpringLayout.NORTH, stochCB, 15, SpringLayout.NORTH,main);
	    lay.putConstraint(SpringLayout.WEST, boolCB, 70, SpringLayout.EAST,stochCB);
   	    lay.putConstraint(SpringLayout.NORTH, boolCB, 15, SpringLayout.NORTH,main);
   	    
   	    lay.putConstraint(SpringLayout.WEST, rosen, 5, SpringLayout.WEST,odeCB);
   	    lay.putConstraint(SpringLayout.NORTH, rosen, 10, SpringLayout.SOUTH,odeCB);
   	    lay.putConstraint(SpringLayout.WEST, rk, 5, SpringLayout.WEST,odeCB);
   	    lay.putConstraint(SpringLayout.NORTH, rk, 10, SpringLayout.SOUTH,rosen);	
   	    
   	    
   	    lay.putConstraint(SpringLayout.WEST, tp, 5, SpringLayout.WEST,stochCB);
   	    lay.putConstraint(SpringLayout.NORTH, tp, 10, SpringLayout.SOUTH,stochCB);
   	    lay.putConstraint(SpringLayout.WEST, gille, 5, SpringLayout.WEST,stochCB);
   	    lay.putConstraint(SpringLayout.NORTH, gille, 10, SpringLayout.SOUTH,tp); 
   	    
   	    JSeparator sep2=new JSeparator(SwingConstants.HORIZONTAL);
	    sep2.setPreferredSize(new Dimension(480, 580));
	   	main.add(sep2);
	    lay.putConstraint(SpringLayout.WEST, sep2, 0, SpringLayout.WEST,main);
   	    lay.putConstraint(SpringLayout.NORTH, sep2, 20, SpringLayout.SOUTH,rk);
   	    
	    lay.putConstraint(SpringLayout.WEST, showMols, 20, SpringLayout.WEST,main);
	    lay.putConstraint(SpringLayout.NORTH, showMols, 50,SpringLayout.SOUTH,rk);
	    lay.putConstraint(SpringLayout.WEST, listScroller, 20, SpringLayout.WEST,main);
	    lay.putConstraint(SpringLayout.NORTH, listScroller, 10,SpringLayout.SOUTH,showMols);
	    
	    lay.putConstraint(SpringLayout.WEST, showMacros, 30, SpringLayout.EAST,listScroller);
	    lay.putConstraint(SpringLayout.NORTH, showMacros,50,SpringLayout.SOUTH,gille);
	    lay.putConstraint(SpringLayout.WEST, listScroller1, 30, SpringLayout.EAST,listScroller);
	    lay.putConstraint(SpringLayout.NORTH, listScroller1, 10,SpringLayout.SOUTH,showMacros);
	    
	    lay.putConstraint(SpringLayout.WEST, time, 20, SpringLayout.WEST,main);
	    lay.putConstraint(SpringLayout.NORTH, time, 40, SpringLayout.SOUTH,listScroller);
	    lay.putConstraint(SpringLayout.WEST, timeText, 5, SpringLayout.EAST,time);
	    lay.putConstraint(SpringLayout.NORTH, timeText, 0, SpringLayout.NORTH,time);
	    lay.putConstraint(SpringLayout.WEST, steps, 40, SpringLayout.EAST,timeText);
	    lay.putConstraint(SpringLayout.NORTH, steps, 0,SpringLayout.NORTH,time);
	    lay.putConstraint(SpringLayout.WEST, stepsText, 10, SpringLayout.EAST,steps);
	    lay.putConstraint(SpringLayout.NORTH, stepsText, 0, SpringLayout.NORTH,steps);		      
	    
	      
	    lay.putConstraint(SpringLayout.WEST, steps_doubling, 15, SpringLayout.WEST,main);
	    lay.putConstraint(SpringLayout.NORTH, steps_doubling, 15,SpringLayout.SOUTH,time);
	    lay.putConstraint(SpringLayout.WEST, stepsDoubling, 5, SpringLayout.EAST,steps_doubling);
	    lay.putConstraint(SpringLayout.NORTH, stepsDoubling, 17,SpringLayout.SOUTH,time);
	      
	    lay.putConstraint(SpringLayout.WEST, convFactor, 0, SpringLayout.WEST,steps);
	    lay.putConstraint(SpringLayout.NORTH, convFactor, 0,SpringLayout.NORTH,steps_doubling);
	    lay.putConstraint(SpringLayout.WEST, convFText, 10, SpringLayout.EAST,convFactor);
	    lay.putConstraint(SpringLayout.NORTH, convFText, 0, SpringLayout.NORTH,convFactor);	      
	    
	    
	    lay.putConstraint(SpringLayout.WEST, crThreshold, 0, SpringLayout.WEST,time);
	    lay.putConstraint(SpringLayout.NORTH, crThreshold, 15,SpringLayout.SOUTH,steps_doubling);
	    lay.putConstraint(SpringLayout.WEST, threshold, 5, SpringLayout.EAST,crThreshold);
	    lay.putConstraint(SpringLayout.NORTH, threshold,  15,SpringLayout.SOUTH,steps_doubling);
	     
	    JSeparator sep4=new JSeparator(SwingConstants.HORIZONTAL);
	    sep4.setPreferredSize(new Dimension(480, 580));
	   	main.add(sep4);
	   	lay.putConstraint(SpringLayout.WEST, sep4, 0, SpringLayout.WEST,main);
   	    lay.putConstraint(SpringLayout.NORTH, sep4, 50, SpringLayout.SOUTH,threshold);
	     contents.add(main,BorderLayout.CENTER);
	}




	/**
	 * 
	 */
	private void createButtonsPanel() {
		buttons=new JPanel();
	    buttons.setBackground(Utils.backgroundColor);
		JButton ok=new JButton("OK");
		ok.setActionCommand("OK");
		ok.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});
		JButton cancel=new JButton("CANCEL");
		cancel.setActionCommand("CANCEL");
		cancel.addActionListener(new ActionListener() {public void actionPerformed(ActionEvent event) { windowAction(event);}});
		FlowLayout lay=new FlowLayout();
		lay.setHgap(40);
		lay.setVgap(20);
		buttons.setLayout(lay);
		buttons.add(ok);
		buttons.add(cancel);
		contents.add(buttons,BorderLayout.SOUTH);
	}













	protected void windowAction(ActionEvent actionCommand) {
		
		
				    
		 String cmd = null;
	      if (actionCommand != null) {
	         if (actionCommand instanceof ActionEvent) {
	            cmd = ((ActionEvent)actionCommand).getActionCommand();
	         } else {
	            cmd = actionCommand.toString();
	         }
	      }
	      if (cmd!=null) {
	    	  if(cmd.equals("OK")){
	    		  
	    		  String method="";
	    		  
	    		  if(odeCB.isSelected()){
	    			  
	    			  model.setSimulationType("ODE");
	    			  model.setOdeCounter(model.getOdeCounter()+1);
	    			  
	    			  if(rk.isSelected()){
	    				  method="numerical_method(rk).\n";	
	    				  model.simulationMap.put("numMethod", "rk");
	    			  }else{
	    				  method="numerical_method(stiff).\n";	
	    				  model.simulationMap.put("numMethod", "stiff");
	    			  }
	    			  if(stepsText.getText()!=null){
	    				  
	    				  try{
	    					  float f=Float.parseFloat(stepsText.getText());
	    					  model.simulationMap.put("numSteps",stepsText.getText());
	    					  method+="step_size("+f+").\n";
	    				  }catch(Exception e){}
			    	  }			    	  
			    	  if(stepsDoubling.getText()!=null){
			    		  
			    		  try{
			    			  float f=Float.parseFloat(stepsDoubling.getText());
			    			  model.simulationMap.put("stepsDoubling",stepsDoubling.getText());
			    			  method+="step_doubling("+f+").\n";
			    		  }catch(Exception e){}
			    	  }else{
			    		  method+="no_step_doubling.\n";
			    	  }
			    	  
			    	  Object[] molecules=((JList)listScroller.getViewport().getView()).getSelectedValues();
			    	  Object[] macros=((JList)listScroller1.getViewport().getView()).getSelectedValues();
			    	  int len1=molecules.length;
			    	  ArrayList<String> selected=new ArrayList<String>();
			    	  for(int i=0;i<len1;i++){
			    		  selected.add((String)molecules[i]);
			    	  }
			    	  ArrayList<String> all=new ArrayList<String>();
			    	  
			    	  MoleculesModel ptMolecules=((ParamTableMolecules)model.getMolecules().getParamTable()).getMoleculesModel();
				      int molSize=ptMolecules.getMolecules().size();
				      
				      
			    	  for(int i=0;i<molSize;i++){
			    		  all.add(ptMolecules.getMolecules().get(i));
			    	  }
			    	  method+="show_molecules({";
			    	  for(int i=0;i<all.size();i++){
			    		  method+=all.get(i);
			    		  if(i<all.size()-1){
			    			  method+=",";
			    		  }
			    	  }
			    	  method+="}).\n";
			    	  
			    	  all.removeAll(selected);
			    	  
			    	  
			    	  if(selected.size()>0 && all.size()>0){
			    		  method+="hide_molecules({";
				    	  for(int i=0;i<all.size();i++){
				    		  method+=all.get(i);
				    		  if(i<all.size()-1){
				    			  method+=",";
				    		  }
				    	  }
				    	  method+="}).\n";
			    	  }else if(all.size()>0 && selected.size()==0){
			    		  
			    		  method+="show_molecules({";
				    	  for(int i=0;i<all.size();i++){
				    		  method+=all.get(i);
				    		  if(i<all.size()-1){
				    			  method+=",";
				    		  }
				    	  }
				    	  method+="}).\n";
			    	  }
			    	  
			    	  
			    	  int len2=macros.length;//macros to be shown...
			    	 
			    	  if(len2>0){    	   	 	  
			    		  method+="show_macros({";
				    	  for(int i=0;i<len2;i++){
				    		  method+=(String)macros[i];
				    		  if(i<len2-1){
				    			  method+=",";
				    		  }
				    	  }
				    	  method+="}).\n";   	  
			    	  }
			    	  
			    	  if(timeText.getText()!=null){
			    		  
			    		  try{
			    			  int f=Integer.parseInt(timeText.getText());
			    			  model.simulationMap.put("lastSim",timeText.getText());	
			    			  method+="numerical_simulation("+f+").\n";
			    		  }catch(Exception e){
			    			  method+="numerical_simulation.\n";
			    		  }
			    	  }else{
			    		  method+="numerical_simulation.\n";
			    	  }

			    	  method+="plot.\n";
	    			  
	    		  }else if(stochCB.isSelected()){
	    			  
	    			  model.setSimulationType("Stochastic");
	    			  model.setStochCounter(model.getStochCounter()+1);
	    			  
	    			  if(tp.isSelected()){
	    				  method="numerical_method(tl).\n";	
	    				  model.simulationMap.put("numMethod", "tl");
	    			  }else{
	    				  method="numerical_method(ssa).\n";	
	    				  model.simulationMap.put("numMethod", "ssa");
	    			  }
	    			  if(convFText.getText()!=null){			    		 
	    				  try{
	    					  int f=Integer.parseInt(convFText.getText());
	    		    		  model.simulationMap.put("convFact",convFText.getText());
	    		    		  method+="conversion_factor("+f+").\n";
	    				  }catch(Exception e){}
			    	  }			    	  
			    	  if(threshold.getText()!=null){
			    		  try{
			    			  int f=Integer.parseInt(threshold.getText());
				    		  model.simulationMap.put("threshold",threshold.getText());
				    		  method+="critical_reaction_threshold("+f+").\n";
			    		  }catch(Exception e){}
			    		  
			    	  }
			    	  
			    	  Object[] molecules=((JList)listScroller.getViewport().getView()).getSelectedValues();
			    	  Object[] macros=((JList)listScroller1.getViewport().getView()).getSelectedValues();
			    	  int len1=molecules.length;
			    	  ArrayList<String> selected=new ArrayList<String>();
			    	  for(int i=0;i<len1;i++){
			    		  selected.add((String)molecules[i]);
			    	  }
			    	  ArrayList<String> all=new ArrayList<String>();
			    	  
			    	  MoleculesModel ptMolecules=((ParamTableMolecules)model.getMolecules().getParamTable()).getMoleculesModel();
				      int molSize=ptMolecules.getMolecules().size();
				      
				      
			    	  for(int i=0;i<molSize;i++){
			    		  all.add(ptMolecules.getMolecules().get(i));
			    	  }
			    	  method+="show_molecules({";
			    	  for(int i=0;i<all.size();i++){
			    		  method+=all.get(i);
			    		  if(i<all.size()-1){
			    			  method+=",";
			    		  }
			    	  }
			    	  method+="}).\n";
			    	  
			    	  all.removeAll(selected);
			    	  
			    	  
			    	  if(selected.size()>0 && all.size()>0){
			    		  method+="hide_molecules({";
				    	  for(int i=0;i<all.size();i++){
				    		  method+=all.get(i);
				    		  if(i<all.size()-1){
				    			  method+=",";
				    		  }
				    	  }
				    	  method+="}).\n";
			    	  }else if(all.size()>0 && selected.size()==0){
			    		  
			    		  method+="show_molecules({";
				    	  for(int i=0;i<all.size();i++){
				    		  method+=all.get(i);
				    		  if(i<all.size()-1){
				    			  method+=",";
				    		  }
				    	  }
				    	  method+="}).\n";
			    	  }
			    	  
			    	  
			    	  int len2=macros.length;//macros to be shown...
			    	 
			    	  if(len2>0){    	   	 	  
			    		  method+="show_macros({";
				    	  for(int i=0;i<len2;i++){
				    		  method+=(String)macros[i];
				    		  if(i<len2-1){
				    			  method+=",";
				    		  }
				    	  }
				    	  method+="}).\n";   	  
			    	  }
			    	  
			    	  if(timeText.getText()!=null){
			    		  
			    		  try{
			    			  int f=Integer.parseInt(timeText.getText());
			    			  model.simulationMap.put("lastSim",timeText.getText());	
			    			  method+="numerical_simulation("+f+").\n";
			    		  }catch(Exception e){
			    			  method+="numerical_simulation.\n";
			    		  }
			    	  }else{
			    		  method+="numerical_simulation.\n";
			    	  }

			    	  method+="plot.\n";
	    			  
	    			  
	    		  }else if(boolCB.isSelected()){
	    			  
	    			 
	    			  Object[] molecules=((JList)listScroller.getViewport().getView()).getSelectedValues();			    	 
			    	  int len1=molecules.length;
			    	  ArrayList<String> selected=new ArrayList<String>();
			    	  for(int i=0;i<len1;i++){
			    		  selected.add((String)molecules[i]);
			    	  }
			    	  ArrayList<String> all=new ArrayList<String>();			    	  
			    	  MoleculesModel ptMolecules=((ParamTableMolecules)model.getMolecules().getParamTable()).getMoleculesModel();
				      int molSize=ptMolecules.getMolecules().size();     
			    	  for(int i=0;i<molSize;i++){
			    		  all.add(ptMolecules.getMolecules().get(i));
			    	  }
			    	  method="show_molecules({";
			    	  for(int i=0;i<all.size();i++){
			    		  method+=all.get(i);
			    		  if(i<all.size()-1){
			    			  method+=",";
			    		  }
			    	  }
			    	  method+="}).\n";			    	  
			    	  all.removeAll(selected);    	  
			    	  if(selected.size()>0 && all.size()>0){
			    		  method+="hide_molecules({";
				    	  for(int i=0;i<all.size();i++){
				    		  method+=all.get(i);
				    		  if(i<all.size()-1){
				    			  method+=",";
				    		  }
				    	  }
				    	  method+="}).\n";
			    	  }else if(all.size()>0 && selected.size()==0){
			    		  
			    		  method+="show_molecules({";
				    	  for(int i=0;i<all.size();i++){
				    		  method+=all.get(i);
				    		  if(i<all.size()-1){
				    			  method+=",";
				    		  }
				    	  }
				    	  method+="}).\n";
			    	  }
			    	  model.setSimulationType("Boolean");
	    			  model.setBoolCounter(model.getBoolCounter()+1);
	    			  method+="boolean_simulation";
	    			  if(timeText.getText()!=""){
	    				  method+="("+timeText.getText()+").\n";
	    			  }else{
	    				  method+=".\n";
	    			  }
			    	  method+="plot.\n";
			    	  int f=Integer.parseInt(timeText.getText());
	    			  model.simulationMap.put("lastSim",timeText.getText());	
	    		  }
	    		
	    		  
	    		  model.executeNumSimulation(method);
	    		  
	    	  }
	    	  setVisible(false);
		      dispose();
	      }
		
	}




	

	
}

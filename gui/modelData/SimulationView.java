package fr.inria.contraintes.biocham.modelData;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import java.util.Map;

import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.customComponents.DnDTabbedPane;
import fr.inria.contraintes.biocham.customComponents.UniversalScrollPane;
import fr.inria.contraintes.biocham.customComponents.CustomPlaf.MyPlafTabbedPane;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSplitPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.SpringLayout;
import javax.swing.text.JTextComponent;
import javax.swing.JProgressBar;
import org.jdesktop.swingx.MultiSplitLayout;
import org.jdesktop.swingx.MultiSplitPane;



/**
 * Class that represents the Simulations part of the GUI.s
 * 
 **/
public class SimulationView extends JSplitPane implements IView{

	
	
	BiochamModel model;
	public DnDTabbedPane tabbedPane;		
	public SimulationsPanel parental;
	public Plotting_Panel plottingPanel;
	SimulationView thisInstance;
	JFrame parentFrame;
	JProgressBar progressBar_ODE,progressBar_Stochastic,progressBar_Boolean;	

	public SimulationView(JFrame f,BiochamModel m){
		
		super();	
		
		model=m;	
		parentFrame=f;
		parental=new SimulationsPanel();
		tabbedPane = new DnDTabbedPane();
		tabbedPane.setOpaque(true);
		tabbedPane.setUI(new MyPlafTabbedPane());
		tabbedPane.setName("tabbedPane");
		tabbedPane.setBackground(Utils.backgroundColor);
		tabbedPane.setFocusable(false);
		tabbedPane.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);			
	
		//Simulators
		ParametersPanel simulators=new ParametersPanel("Simulators");
		simulators.setLayout(new BorderLayout());
		
		//ode
		ODE_Panel odeSim=new ODE_Panel();		
		//stochastic
		Stochastic_Panel stochasticSim=new Stochastic_Panel();			
		//boolean
		Boolean_Panel booleanSim=new Boolean_Panel();
		//Plotting
		plottingPanel=new Plotting_Panel();	
		
		
		StringBuffer bf=new StringBuffer();
		bf.append("(COLUMN ");		
		bf.append(odeSim.toString());
		bf.append(" ");
		bf.append(stochasticSim.toString());
		bf.append(" ");
		bf.append(booleanSim.toString());
		bf.append(" ");
		bf.append(plottingPanel.toString());
		bf.append(" ");
		bf.append(")");			
		MultiSplitLayout.Node modelRoot = MultiSplitLayout.parseModel(bf.toString());
		bf=null;
		
		MultiSplitPane multiSplitPane = new MultiSplitPane();	
		multiSplitPane.setDividerSize(5);
		multiSplitPane.setContinuousLayout(true);	
		multiSplitPane.getMultiSplitLayout().setModel(modelRoot);		
		
		
		multiSplitPane.add(odeSim, odeSim.toString());
		multiSplitPane.add(stochasticSim, stochasticSim.toString());
		multiSplitPane.add(booleanSim, booleanSim.toString());
		multiSplitPane.add(plottingPanel, plottingPanel.toString());
		
		multiSplitPane.setPreferredSize(new Dimension(300,900));
		simulators.add(multiSplitPane,BorderLayout.CENTER);
		
		super.setOrientation(JSplitPane.HORIZONTAL_SPLIT);
		super.setLeftComponent((new JScrollPane(simulators)));
		super.setRightComponent(tabbedPane);		
		setResizeWeight(0.1);  
		setDividerLocation(400);
		
		model.getSimulationModel().getViews().add(this);
		thisInstance=this;
		refresh();
	}
	public class Plotting_Panel extends JPanel{
		
		UniversalScrollPane listScroller1,listScroller2,listScroller3;
		JList molList,macroList,paramList;
		
		public void rebuild(){
			MoleculesModel molsModel=((ParamTableMolecules)model.getMolecules().getParamTable()).getMoleculesModel();
			int size=molsModel.getMolecules().size();	
			Utils.debugMsg(size);
			String molecules[]=new String[size];
			for(int h=0;h<size;h++){
				molecules[h]=molsModel.getMolecules().get(h);
			}
			molList=new JList(molecules);
			molList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
			molList.setLayoutOrientation(JList.VERTICAL);
			molList.setVisibleRowCount(1);
			molList.setToolTipText("<html><u><b>Tip:</b></u> Click on the item to select it<br> Shift+Click for multiple sequence selection <br> Ctrl+Click for multiple non-sequence selection <br> Ctrl+Click on the item to deselect it.</html>");	
			listScroller1.setViewportView(molList);
			
			MacrosModel macrosModel=((ParamTableMacros)model.getMacros().getParamTable()).getMacrosModel();
			int msize=macrosModel.getMacros().size();	
			Utils.debugMsg(msize);
			String macros[]=new String[msize];
			Iterator it = macrosModel.getMacros().keySet().iterator();
			int i=0;
			while(it.hasNext()){				  
				macros[i]=it.next().toString();
				i++;
			}
			it=null;
			macroList=new JList(macros);
			macroList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
			macroList.setLayoutOrientation(JList.VERTICAL);
			macroList.setVisibleRowCount(1);
			macroList.setToolTipText("<html><u><b>Tip:</b></u> Click on the item to select it<br> Shift+Click for multiple sequence selection <br> Ctrl+Click for multiple non-sequence selection <br> Ctrl+Click on the item to deselect it.</html>");	
			listScroller2.setViewportView(macroList);
			
			ParametersModel paramsModel=((ParamTableParameters)model.getParameters().getParamTable()).getParametersModel();
			int sizep=paramsModel.getParameters().size();	
			Utils.debugMsg(sizep);
			String parameters[]=new String[sizep];
			for(int h=0;h<sizep;h++){
				parameters[h]=paramsModel.getParametersNames().get(h);
			}
			paramList=new JList(parameters);
			paramList.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION);
			paramList.setLayoutOrientation(JList.VERTICAL);
			paramList.setVisibleRowCount(1);
			paramList.setToolTipText("<html><u><b>Tip:</b></u> Click on the item to select it<br> Shift+Click for multiple sequence selection <br> Ctrl+Click for multiple non-sequence selection <br> Ctrl+Click on the item to deselect it.</html>");	
			listScroller3.setViewportView(paramList);
			
			setPreferredSize(new Dimension(200,800));
			
		}
		
		public String getShowingProperties(){
			
			String value=null;			
			StringBuffer bf=new StringBuffer();
			if(paramList.getSelectedValues().length>0){
				bf.append("show_parameters({");
				bf.append(paramList.getSelectedValues()[0]);
				for(int i=1;i<paramList.getSelectedValues().length;i++){
					bf.append(",");
					bf.append(paramList.getSelectedValues()[i]);
				}
				bf.append("}).\n");
			}else{
				bf.append("hide_parameters.\n");
			}
			if(macroList.getSelectedValues().length>0){
				bf.append("show_macros({");
				bf.append(macroList.getSelectedValues()[0]);
				for(int i=1;i<macroList.getSelectedValues().length;i++){
					bf.append(",");
					bf.append(macroList.getSelectedValues()[i]);
				}
				bf.append("}).\n");
			}else{
				bf.append("hide_macros.\n");
			}
			if(molList.getSelectedValues().length>0){
				/**Shows just some of them, and hides the others...**/
				bf.append("hide_molecules({");
				MoleculesModel molsModel=((ParamTableMolecules)model.getMolecules().getParamTable()).getMoleculesModel();
				int size=molsModel.getMolecules().size();			
				for(int h=0;h<size;h++){					
					bf.append(molsModel.getMolecules().get(h));
					if(h<size-1){
						bf.append(",");
					}
				}
				bf.append("}).\n");
				bf.append("show_molecules({");
				bf.append(molList.getSelectedValues()[0]);
				for(int i=1;i<molList.getSelectedValues().length;i++){
					bf.append(",");
					bf.append(molList.getSelectedValues()[i]);
				}
				bf.append("}).\n");
				
			}else{
				/**Shows ALL by default**/
				bf.append("show_molecules({");
				MoleculesModel molsModel=((ParamTableMolecules)model.getMolecules().getParamTable()).getMoleculesModel();
				int size=molsModel.getMolecules().size();			
				for(int h=0;h<size;h++){					
					bf.append(molsModel.getMolecules().get(h));
					if(h<size-1){
						bf.append(",");
					}
				}
				bf.append("}).\n");
			}
			
			
			value=bf.toString();
			bf=null;
			
			return value;
		}
		
		public Plotting_Panel(){
		
			super();
			setBackground(Utils.backgroundColor);
			SpringLayout sp=new SpringLayout();
			setLayout(sp);
			JLabel showMols=new JLabel("<html><u>Show molecules:</u>");     
			JLabel showMacros=new JLabel("<html><u>Show macros:</u>");
			JLabel showParams=new JLabel("<html><u>Show parameters:</u>");
			add(showMols);
			add(showMacros);
			add(showParams);
			sp.putConstraint(SpringLayout.WEST, showMols, 5, SpringLayout.WEST,this);
			sp.putConstraint(SpringLayout.NORTH, showMols, 10,SpringLayout.NORTH,this);
			
			
			 
			listScroller1 = new UniversalScrollPane(molList);
			listScroller1.setPreferredSize(new Dimension(200, 100));
			add(listScroller1);
			sp.putConstraint(SpringLayout.WEST, listScroller1, 40, SpringLayout.EAST,showMols);
			sp.putConstraint(SpringLayout.NORTH, listScroller1, 10,SpringLayout.NORTH,this);			
			
				      
			listScroller2 = new UniversalScrollPane(macroList);
			listScroller2.setPreferredSize(new Dimension(200, 100));
			add(listScroller2);
			sp.putConstraint(SpringLayout.WEST, showMacros, 5, SpringLayout.WEST,this);
			sp.putConstraint(SpringLayout.NORTH, showMacros, 40,SpringLayout.SOUTH,listScroller1);			
			sp.putConstraint(SpringLayout.WEST, listScroller2, 40, SpringLayout.EAST,showMols);
			sp.putConstraint(SpringLayout.NORTH, listScroller2,  40,SpringLayout.SOUTH,listScroller1);	
			
				
				      
			listScroller3 = new UniversalScrollPane(paramList);
			listScroller3.setPreferredSize(new Dimension(200, 100));
			add(listScroller3);
		
			sp.putConstraint(SpringLayout.WEST, showParams, 5, SpringLayout.WEST,this);
			sp.putConstraint(SpringLayout.NORTH, showParams,40,SpringLayout.SOUTH,listScroller2);
			sp.putConstraint(SpringLayout.WEST, listScroller3, 40, SpringLayout.EAST,showMols);
			sp.putConstraint(SpringLayout.NORTH, listScroller3,  40,SpringLayout.SOUTH,listScroller2);	
			
			//setPreferredSize(new Dimension(200,400));
		}
		
		
		public String toString(){
			return "Plotting";
		}
	}
	class ODE_Panel extends ParametersPanel{
		
		JCheckBox steps_doubling;
		JTextField stepsDoubling;
		JRadioButton rosen,rk;
		
		public ODE_Panel(){
			super("ODE Simulator");
			SpringLayout sp=new SpringLayout();
			setLayout(sp);
			final JTextField stepsText=new JTextField(4);
			stepsText.setEnabled(false);
			ButtonGroup group1 = new ButtonGroup();
			rosen=new JRadioButton("Rosenbrock");
			rosen.setBackground(Utils.backgroundColor);
			rosen.setActionCommand("rosen");
			rosen.setSelected(true);
			
			rk=new JRadioButton("Runge-Kutta");
			rk.setBackground(Utils.backgroundColor);
			rk.setActionCommand("rk");
			rk.setSelected(false);
			
			rosen.addActionListener(new ActionListener(){

				public void actionPerformed(ActionEvent arg0) {
					if(rosen.isSelected()){
						stepsText.setEnabled(false);
						steps_doubling.setSelected(true);
						steps_doubling.setEnabled(false);						
						stepsDoubling.setEnabled(true);
						stepsDoubling.setEditable(true);
					}
					
				}});
			rk.addActionListener(new ActionListener(){

				public void actionPerformed(ActionEvent arg0) {
					if(rk.isSelected()){
						stepsText.setEnabled(true);
						steps_doubling.setEnabled(true);
						steps_doubling.setSelected(true);						
						stepsDoubling.setEnabled(true);
						stepsDoubling.setEditable(true);
					}					
				}});
			group1.add(rosen);		
			group1.add(rk);
			add(rosen);
			add(rk);
			
			sp.putConstraint(SpringLayout.WEST, rosen, 5, SpringLayout.WEST,this);
			sp.putConstraint(SpringLayout.NORTH, rosen, 10, SpringLayout.NORTH,this);
			sp.putConstraint(SpringLayout.WEST, rk, 10, SpringLayout.EAST,rosen);
			sp.putConstraint(SpringLayout.NORTH, rk,  10, SpringLayout.NORTH,this);
			
			
			JLabel time=new JLabel("Time units:");
			add(time);		      
			final JTextField timeText=new JTextField(4);
			String txt=model.simulationMap.get("lastSim");
			if(txt!=null){
				timeText.setText(txt);
			}else{
				timeText.setText("20");
			}
			add(timeText);			    
			JLabel steps=new JLabel("Step size:");
			add(steps);
			
			txt=model.simulationMap.get("numSteps");
			if(txt!=null){
				stepsText.setText(txt);
			}else{
				stepsText.setText("0.01");
			}
			add(stepsText);			      
			steps_doubling=new JCheckBox("Steps Doubling:");
			steps_doubling.setEnabled(false);
			steps_doubling.setBackground(Utils.backgroundColor);
			steps_doubling.addActionListener(new ActionListener(){
				public void actionPerformed(ActionEvent e) {
					Component c=(Component)e.getSource();
					if(c instanceof JCheckBox){
						JCheckBox cb=(JCheckBox)c;
						if(rosen.isSelected()){	
							if(cb.isSelected()){
								stepsDoubling.setEnabled(true);
								stepsDoubling.setEditable(true);
							}else{
								stepsDoubling.setEditable(false);
								stepsDoubling.setEnabled(false);
							}
						}
					}				
				}});
			add(steps_doubling);
			stepsDoubling=new JTextField(4);
			txt=model.simulationMap.get("stepsDoubling");
			if(txt!=null){
				stepsDoubling.setText(txt);
			}else{
				stepsDoubling.setText("0.0001");
			}
			stepsDoubling.setEditable(false);
			add(stepsDoubling);			
			
			if(rosen.isSelected()){
				steps_doubling.setEnabled(false);
				stepsDoubling.setEnabled(true);
				stepsDoubling.setEditable(true);
			}else if(rk.isSelected()){
				steps_doubling.setSelected(true);
				steps_doubling.setEnabled(true);
				stepsDoubling.setEnabled(true);
				stepsDoubling.setEditable(true);
			}		
			
			sp.putConstraint(SpringLayout.WEST, time, 5, SpringLayout.WEST,this);
			sp.putConstraint(SpringLayout.NORTH, time, 20, SpringLayout.SOUTH,rosen);
			sp.putConstraint(SpringLayout.WEST, timeText, 5, SpringLayout.EAST,time);
			sp.putConstraint(SpringLayout.NORTH, timeText, 16, SpringLayout.SOUTH,rosen);
			
			sp.putConstraint(SpringLayout.WEST, steps, 5, SpringLayout.WEST,this);
			sp.putConstraint(SpringLayout.NORTH, steps, 12, SpringLayout.SOUTH,timeText);
			sp.putConstraint(SpringLayout.WEST, stepsText,5, SpringLayout.EAST,time);
			sp.putConstraint(SpringLayout.NORTH, stepsText, 8, SpringLayout.SOUTH,timeText);
			
			sp.putConstraint(SpringLayout.WEST, steps_doubling, 5, SpringLayout.WEST,this);
			sp.putConstraint(SpringLayout.NORTH, steps_doubling, 23, SpringLayout.SOUTH,stepsText);
			sp.putConstraint(SpringLayout.WEST, stepsDoubling, 5, SpringLayout.EAST,steps_doubling);
			sp.putConstraint(SpringLayout.NORTH, stepsDoubling, 20, SpringLayout.SOUTH,stepsText);
			
			JButton run=new JButton("Run");
			progressBar_ODE = new JProgressBar();
			progressBar_ODE.setPreferredSize(new Dimension(80,20));
			progressBar_ODE.setVisible(false);    
			progressBar_ODE.setStringPainted(false);
			add(run);
			add(progressBar_ODE);
			run.addActionListener(new ActionListener(){

				public void actionPerformed(ActionEvent arg0) {
					
					model.setSimulationType("ODE");
					model.setOdeCounter(model.getOdeCounter()+1);

	    			  
					String stepSize=stepsText.getText().trim();
					String time=timeText.getText().trim();
					if(!(stepSize!=null && !stepSize.equals("") && !stepSize.equals(" "))){
						stepSize="0.01";						
					}
					model.simulationMap.put("numSteps",stepSize);
					if(!(time!=null && !time.equals("") && !time.equals(" "))){
						time="20";						
					}
					model.simulationMap.put("lastSim",time);
					String method="stiff";
					if(rk.isSelected()){
						method="rk";
											}
					StringBuffer bf=new StringBuffer();
					bf.append(plottingPanel.getShowingProperties());
					bf.append("numerical_method(");
					bf.append(method);
					bf.append(").\n");
					if(rk.isSelected()){
						bf.append("step_size(");
						bf.append(stepSize);
						bf.append(").\n");
					}
					if(!steps_doubling.isEnabled() || (steps_doubling.isEnabled() && steps_doubling.isSelected())){
						String dbls=stepsDoubling.getText().trim();
						if(!(dbls!=null && !dbls.equals("") && !dbls.equals(" "))){
							dbls="0.0001";						
						}
						bf.append("step_doubling(");
						bf.append(dbls);
						bf.append(").\n");
						model.simulationMap.put("stepsDoubling",dbls);
					}else{
						bf.append("no_step_doubling.\n");
					}
					bf.append("numerical_simulation(");
					bf.append(time);
					bf.append(").\n");
					//bf.append(plottingPanel.getShowingProperties());
					bf.append("plot.\n");
					//bf.append(plottingPanel.getShowingProperties());
					model.sendToBiochamFromsimulations(bf.toString(),thisInstance);
					progressBar_ODE.setVisible(true);
					progressBar_ODE.setIndeterminate(true);		
					bf=null;
					
				}});
			sp.putConstraint(SpringLayout.WEST, run, 15, SpringLayout.EAST,rk);
			sp.putConstraint(SpringLayout.NORTH, run, 10, SpringLayout.NORTH,this);
			sp.putConstraint(SpringLayout.WEST, progressBar_ODE, 5, SpringLayout.EAST,run);
			sp.putConstraint(SpringLayout.NORTH, progressBar_ODE, 15, SpringLayout.NORTH,this);
			setPreferredSize(new Dimension(200,400));
		}
		public String toString(){
			return "ODE";
		}
	}
	class Stochastic_Panel extends ParametersPanel{
		
	
		JTextField convFactor, criticalThreshold;
		JRadioButton ssa,tl;
		
		public Stochastic_Panel(){
			super("Stochastic Simulator");
			
			SpringLayout sp=new SpringLayout();
			setLayout(sp);
			ButtonGroup group1 = new ButtonGroup();
			ssa=new JRadioButton("Gillespie");
			ssa.setBackground(Utils.backgroundColor);
			ssa.setActionCommand("ssa");
			ssa.setSelected(true);
			
			tl=new JRadioButton("Tau-Lipping");
			tl.setBackground(Utils.backgroundColor);
			tl.setActionCommand("tl");
			tl.setSelected(false);
			
			tl.addActionListener(new ActionListener(){

				public void actionPerformed(ActionEvent arg0) {
					if(tl.isSelected()){						
						criticalThreshold.setEnabled(true);
						criticalThreshold.setEditable(true);
					}
					
				}});
			ssa.addActionListener(new ActionListener(){

				public void actionPerformed(ActionEvent arg0) {
					if(ssa.isSelected()){						
						criticalThreshold.setEnabled(false);
						criticalThreshold.setEditable(false);
					}					
				}});
			group1.add(ssa);		
			group1.add(tl);
			add(ssa);
			add(tl);
			
			sp.putConstraint(SpringLayout.WEST, ssa, 5, SpringLayout.WEST,this);
			sp.putConstraint(SpringLayout.NORTH, ssa, 10, SpringLayout.NORTH,this);
			sp.putConstraint(SpringLayout.WEST, tl, 10, SpringLayout.EAST,ssa);
			sp.putConstraint(SpringLayout.NORTH, tl,  10, SpringLayout.NORTH,this);
			
			
			JLabel time=new JLabel("Time units:");
			add(time);		      
			final JTextField timeText=new JTextField(4);
			String txt=model.simulationMap.get("lastSim");
			if(txt!=null){
				timeText.setText(txt);
			}else{
				timeText.setText("20");
			}
			add(timeText);			    
			JLabel conv=new JLabel("Conversion factor:");
			add(conv);
			convFactor=new JTextField(4);
			txt=model.simulationMap.get("convF");
			if(txt!=null){
				convFactor.setText(txt);
			}else{
				convFactor.setText("100");
			}
			add(convFactor);			      
			
			JLabel criticalThs=new JLabel("Reaction Threshold:");
			add(criticalThs);
			criticalThreshold=new JTextField(4);
			txt=model.simulationMap.get("criticalThreshold");
			if(txt!=null){
				criticalThreshold.setText(txt);
			}else{
				criticalThreshold.setText("20");
			}
			criticalThreshold.setEditable(false);
			criticalThreshold.setEnabled(false);
			add(criticalThreshold);			
			
			if(ssa.isSelected()){				
				criticalThreshold.setEnabled(false);
				criticalThreshold.setEditable(false);				
			}else if(tl.isSelected()){				
				criticalThreshold.setEnabled(true);
				criticalThreshold.setEditable(true);
			}		
			
			sp.putConstraint(SpringLayout.WEST, time, 5, SpringLayout.WEST,this);
			sp.putConstraint(SpringLayout.NORTH, time, 20, SpringLayout.SOUTH,ssa);
			sp.putConstraint(SpringLayout.WEST, timeText, 5, SpringLayout.EAST,time);
			sp.putConstraint(SpringLayout.NORTH, timeText, 16, SpringLayout.SOUTH,ssa);
			
			sp.putConstraint(SpringLayout.WEST, conv, 5, SpringLayout.WEST,this);
			sp.putConstraint(SpringLayout.NORTH, conv, 12, SpringLayout.SOUTH,timeText);
			sp.putConstraint(SpringLayout.WEST, convFactor,5, SpringLayout.EAST,conv);
			sp.putConstraint(SpringLayout.NORTH, convFactor, 8, SpringLayout.SOUTH,timeText);
			
			sp.putConstraint(SpringLayout.WEST, criticalThs, 5, SpringLayout.WEST,this);
			sp.putConstraint(SpringLayout.NORTH, criticalThs, 23, SpringLayout.SOUTH,convFactor);
			sp.putConstraint(SpringLayout.WEST, criticalThreshold, 5, SpringLayout.EAST,criticalThs);
			sp.putConstraint(SpringLayout.NORTH, criticalThreshold, 20, SpringLayout.SOUTH,convFactor);
			
			JButton run=new JButton("Run");
			progressBar_Stochastic = new JProgressBar();
			progressBar_Stochastic.setPreferredSize(new Dimension(80,20));
			progressBar_Stochastic.setVisible(false);    
			progressBar_Stochastic.setStringPainted(false);
			add(run);
			add(progressBar_Stochastic);	
			run.addActionListener(new ActionListener(){

				public void actionPerformed(ActionEvent arg0) {
					
					model.setSimulationType("Stochastic");
					model.setOdeCounter(model.getStochCounter()+1);

	    			  
					String conv=convFactor.getText().trim();
					String time=timeText.getText().trim();
					if(!(conv!=null && !conv.equals("") && !conv.equals(" "))){
						conv="10000";						
					}
					model.simulationMap.put("convF",conv);
					if(!(time!=null && !time.equals("") && !time.equals(" "))){
						time="20";						
					}
					model.simulationMap.put("lastSim",time);
					String method="ssa";
					if(tl.isSelected()){
						method="tl";
					}
					StringBuffer bf=new StringBuffer();
					bf.append("numerical_method(");
					bf.append(method);
					bf.append(").\n");
					bf.append("conversion_factor(");
					bf.append(conv);
					bf.append(").\n");
					if(tl.isSelected()){
						String ct=criticalThreshold.getText().trim();
						if(!(ct!=null && !ct.equals("") && !ct.equals(" "))){
							ct="20";						
						}
						bf.append("critical_reaction_threshold(");
						bf.append(ct);
						bf.append(").\n");
						model.simulationMap.put("criticalThreshold",ct);
					}
					bf.append("numerical_simulation(");
					bf.append(time);
					bf.append(").\n");
					bf.append(plottingPanel.getShowingProperties());
					bf.append("plot.\n");
					model.sendToBiochamFromsimulations(bf.toString(),thisInstance);
					progressBar_Stochastic.setVisible(true);
					progressBar_Stochastic.setIndeterminate(true);	
					bf=null;
					
				}});
			sp.putConstraint(SpringLayout.WEST, run, 15, SpringLayout.EAST,tl);
			sp.putConstraint(SpringLayout.NORTH, run, 10, SpringLayout.NORTH,this);
			sp.putConstraint(SpringLayout.WEST, progressBar_Stochastic, 5, SpringLayout.EAST,run);
			sp.putConstraint(SpringLayout.NORTH, progressBar_Stochastic, 15, SpringLayout.NORTH,this);
			setPreferredSize(new Dimension(200,400));
			
		}
		public String toString(){
			return "Stochastic";
		}
	}
	class Boolean_Panel extends ParametersPanel{
		public Boolean_Panel(){
			super("Boolean Simulator");			
			SpringLayout sp=new SpringLayout();
			setLayout(sp);
			JLabel l=new JLabel("Number of transitions: ");
			final JTextField tf=new JTextField(5);
			String ts=model.simulationMap.get("lastSimBool");
			if(ts!=null){
				tf.setText(ts);
			}else{
				tf.setText("30");	
			}						
			JButton run=new JButton("Run");
			progressBar_Boolean = new JProgressBar();
			progressBar_Boolean.setPreferredSize(new Dimension(50,15));
			progressBar_Boolean.setVisible(false);    
			progressBar_Boolean.setStringPainted(false);
			add(l);
			add(tf);
			add(run);
			add(progressBar_Boolean);
			run.addActionListener(new ActionListener(){

				public void actionPerformed(ActionEvent arg0) {
					model.setSimulationType("Boolean");
					model.setBoolCounter(model.getBoolCounter()+1);
					String nbTransitions=tf.getText().trim();
					if(!(nbTransitions!=null && !nbTransitions.equals("") && !nbTransitions.equals(" "))){
						nbTransitions="30";						
					}
					model.simulationMap.put("lastSimBool",nbTransitions);
					StringBuffer bf=new StringBuffer();
					bf.append("boolean_simulation(");
					bf.append(nbTransitions);
					bf.append(").\n");
					bf.append(plottingPanel.getShowingProperties());
					bf.append("plot.\n");
					model.sendToBiochamFromsimulations(bf.toString(),thisInstance);
					progressBar_Boolean.setVisible(true);
					progressBar_Boolean.setIndeterminate(true);		
					bf=null;
					
				}});
			sp.putConstraint(SpringLayout.WEST, l, 5, SpringLayout.WEST,this);
			sp.putConstraint(SpringLayout.NORTH, l,12,SpringLayout.NORTH,this);
			sp.putConstraint(SpringLayout.WEST, tf, 10, SpringLayout.EAST,l);
			sp.putConstraint(SpringLayout.NORTH, tf,10,SpringLayout.NORTH,this);
			sp.putConstraint(SpringLayout.WEST, run, 10, SpringLayout.EAST,tf);
			sp.putConstraint(SpringLayout.NORTH, run,9,SpringLayout.NORTH,this);
			sp.putConstraint(SpringLayout.WEST, progressBar_Boolean, 5, SpringLayout.EAST,run);
			sp.putConstraint(SpringLayout.NORTH, progressBar_Boolean, 15, SpringLayout.NORTH,this);
			setPreferredSize(new Dimension(100,150));
		}
		public String toString(){
			return "Boolean";
		}
	}
	
	/**
	 * Class that represents the Simulation INTRO Panel, when there are not yet any simulations being executed.  
	 */
	public class SimulationsPanel extends JPanel{
		ImageIcon img = Icons.icons.get("chart-icon2.png");
		public SimulationsPanel(){
			super();		
			setBackground(Color.white);
			setLayout(new BorderLayout());
			JPanel p=new JPanel();
			p.setLayout(new BoxLayout(p, BoxLayout.PAGE_AXIS));
			p.setBackground(Color.white);			
			JLabel l=new JLabel(img);
			p.add(Box.createVerticalGlue());		
			Dimension minSize = new Dimension(200, 10);
			Dimension prefSize = new Dimension(100, 10);
			Dimension maxSize = new Dimension(Short.MAX_VALUE, 10);		
			p.add(new Box.Filler(minSize, prefSize, maxSize));
			p.add(l);
			p.add(Box.createHorizontalGlue());
			p.add(Box.createVerticalGlue());
			add(p,BorderLayout.CENTER);
		}		
	}
	
	
	public BiochamModel getModel() {
		return model;
	}
	public void setModel(BiochamModel model) {
		this.model = model;
	}
	public String toString(){
		return "Simulations";
	}
	public DnDTabbedPane getTabbedPane() {
		return tabbedPane;
	}
	public void setTabbedPane(DnDTabbedPane tabbedPane) {
		this.tabbedPane = tabbedPane;
	}
	public Plotting_Panel getPlottingPanel() {
		return plottingPanel;
	}
	public JFrame getParentFrame() {
		return parentFrame;
	}
	public void setParentFrame(JFrame parentFrame) {
		this.parentFrame = parentFrame;
	}
	public void refresh() {
		plottingPanel.rebuild();
		
	}

	public void setProgressBarDone(){
		progressBar_ODE.setValue(100);
		progressBar_ODE.setVisible(false);
		progressBar_Stochastic.setValue(100);
		progressBar_Stochastic.setVisible(false);
		progressBar_Boolean.setValue(100);
		progressBar_Boolean.setVisible(false);
	}
}

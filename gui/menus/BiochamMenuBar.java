package fr.inria.contraintes.biocham.menus;
import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamMainFrame;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.WorkbenchActionListener;
import fr.inria.contraintes.biocham.utils.BrowserLauncher;
import fr.inria.contraintes.biocham.utils.Icons;
import fr.inria.contraintes.biocham.utils.Utils;

import fr.inria.contraintes.biocham.modelData.AbstractionView;
import fr.inria.contraintes.biocham.modelData.CTLView;
import fr.inria.contraintes.biocham.modelData.DeclarationsView;
import fr.inria.contraintes.biocham.modelData.EventsView;
import fr.inria.contraintes.biocham.modelData.InitialStateView;
import fr.inria.contraintes.biocham.modelData.InvariantsView;
import fr.inria.contraintes.biocham.modelData.LTLView;
import fr.inria.contraintes.biocham.modelData.MacrosView;
import fr.inria.contraintes.biocham.modelData.MoleculesView;
import fr.inria.contraintes.biocham.modelData.ParamTableCTLSpecifications;
import fr.inria.contraintes.biocham.modelData.ParamTableDeclarations;
import fr.inria.contraintes.biocham.modelData.ParamTableEvents;
import fr.inria.contraintes.biocham.modelData.ParamTableLTLSpecifications;
import fr.inria.contraintes.biocham.modelData.ParamTableMolecules;
import fr.inria.contraintes.biocham.modelData.ParamTableConservationLaws;
import fr.inria.contraintes.biocham.modelData.ParamTableInitConc;
import fr.inria.contraintes.biocham.modelData.ParamTableParameters;
import fr.inria.contraintes.biocham.modelData.ParamTableMacros;
import fr.inria.contraintes.biocham.modelData.ParamTableRules;
import fr.inria.contraintes.biocham.modelData.ParamTableVolumes;
import fr.inria.contraintes.biocham.modelData.ParametersView;
import fr.inria.contraintes.biocham.modelData.RulesView;
import fr.inria.contraintes.biocham.modelData.SimulationView;
import fr.inria.contraintes.biocham.modelData.VolumesView;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.KeyEvent;

import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JScrollPane;
import javax.swing.JToolBar;
import javax.swing.KeyStroke;



/**
 * 
 * A class representing the main MenuBar of the GUI. It contains the Biocham item, model's item and the Help item.
 * 
 * @author Dragana Jovanovska 
 * 
 */
public class BiochamMenuBar implements ActionListener{

	
	

	public static JMenuBar menuBar;
    public static JToolBar toolBar;
    public static JMenu helpMenu,commandsMenu;
    public static JCheckBoxMenuItem checkForUpdatesBox;	
    public static JMenuItem menuItem;
    public static JMenuItem model;
    public static ColorMenu modelMenu,abstractionMenu;
	JMenu rulesMenu,initialMenu,parametersMenu, declarationMenu, invariantsMenu, macrosMenu, volumesMenu,moleculesMenu, eventsMenu;
	public static ElementMenuModel modelM;
	public static ElementMenuAbstractions absM;
	    

	/**
	 * 
	 * Build the menu bar with its items and sub-items.
	 *  
	 */
    public JMenuBar createMenuBar(BiochamDynamicTree tree){
    	
    	    	
    	if(menuBar==null){
    		
		    menuBar=new JMenuBar();
			menuBar.setMargin(new Insets(40,0,40,0));
			menuBar.setForeground(Utils.foregroundColor);
			menuBar.setBackground(Utils.backgroundColor);
			menuBar.setName("workbenchMenuBar");		
			modelMenu = BiochamMenuBar.createNewMenu("Biocham");		
			modelMenu.setBackground(Utils.backgroundColor);
			
			ImageIcon bwIcon =Icons.icons.get("article-32.png");
			menuItem=createMenuItem("New Model",KeyEvent.VK_N,bwIcon);
			menuItem.setActionCommand("newBCmodel");
		    menuItem.setToolTipText("Create new Biocham Model...");
		    menuItem.addActionListener(tree.treeListener);		
		    modelMenu.add(menuItem);
		    
		    bwIcon =Icons.icons.get("folderblue.png");
		    menuItem=createMenuItem("Open Model",KeyEvent.VK_O,bwIcon);
		    menuItem.setToolTipText("Open existing Biocham Model...");
		    menuItem.setActionCommand("openBCmodel");
		    menuItem.addActionListener(tree.treeListener);			
		    modelMenu.add(menuItem);	    
		    modelMenu.addSeparator();   
		    
		    bwIcon =Icons.icons.get("explorer-32.png");
		    checkForUpdatesBox = new JCheckBoxMenuItem("  Check for updates", bwIcon,true);
		    checkForUpdatesBox.setBackground(Utils.backgroundColor);
		    checkForUpdatesBox.setFont(new Font("",Font.BOLD,12));
		    checkForUpdatesBox.setForeground(Utils.foregroundColor);		  
		    checkForUpdatesBox.addItemListener(new ItemListener(){
	
				public void itemStateChanged(ItemEvent e) {
					if(e.getStateChange()==ItemEvent.SELECTED){
						BiochamMainFrame.checkForUpdatesBox.setSelected(true);
						BiochamMainFrame.prefs.putBoolean("checkForUpdates", true);
					}else{
						BiochamMainFrame.checkForUpdatesBox.setSelected(false);
						checkForUpdatesBox.setSelected(false);
						BiochamMainFrame.prefs.putBoolean("checkForUpdates", false);
					}				
				}});
		    boolean s=BiochamMainFrame.prefs.getBoolean("checkForUpdates", true);
		    checkForUpdatesBox.setSelected(s);	 
		    modelMenu.add(checkForUpdatesBox);	    
		    if (System.getProperty("mrj.version") == null) { // not Mac
		    	  
		    	bwIcon =Icons.icons.get("remove3.png");
		    	modelMenu.addSeparator(); 	
		    	menuItem=createMenuItem("   Quit",KeyEvent.VK_Q,bwIcon);
			    menuItem.setToolTipText("Close this application");
			    menuItem.addActionListener(new ActionListener(){				
					public void actionPerformed(ActionEvent e) {
						BiochamMainFrame.quit();
						
					}});
			    modelMenu.add(menuItem);		         
		    } else { // Mac OS
		         //System.setProperty("com.apple.macos.useScreenMenuBar", "true");
		         //System.setProperty("com.apple.mrj.application.growbox.intrudes",
		         //      "false");
		         //System.setProperty("com.apple.mrj.application.apple.menu.about.name",
		         //      "Biocham");
		    }   	    
		    menuBar.add(modelMenu);		   
		  //  modelMenu=tree.modelMenu.getMenu();			
		    
		   
			modelM=(ElementMenuModel)tree.modelMenu;
			modelM.getMenu().setEnabled(false);
			//absM=(ElementMenuAbstractions)tree.abstractionMenu;
		    menuBar.add(modelM.getMenu());		   
		    helpMenu = createNewMenu("Help");  	    
		    bwIcon =Icons.icons.get("computer_48.png" +0.5);
		    menuItem=createMenuItem(" Documentation",KeyEvent.VK_L,bwIcon);
		    menuItem.setToolTipText("Browse (local) Documentation and Tutorials");
		    WorkbenchActionListener t=new WorkbenchActionListener("documentation");
		    menuItem.addActionListener(t);	  
		    helpMenu.add(menuItem);  
		   	    
		    bwIcon =Icons.icons.get("agt_internet-32.png" +0.8);
		    menuItem=createMenuItem(" Biocham Web Page",KeyEvent.VK_W,bwIcon);
		    menuItem.setToolTipText("Browse (online) Tutorial");	
		    menuItem.addActionListener(new ActionListener(){			
				public void actionPerformed(ActionEvent e) {
					Thread th=new Thread(new Runnable(){
	
						public void run() {
							BrowserLauncher.openURL("http://contraintes.inria.fr/BIOCHAM");
						}
					});
					th.start();
				}	
		    });
		    helpMenu.add(menuItem);
		    if (System.getProperty("mrj.version") == null) { // not Mac
		    	
		    	helpMenu.addSeparator();
		    	bwIcon =Icons.icons.get("Info_32.png");
		    	menuItem=createMenuItem("About",KeyEvent.VK_A,bwIcon);
			    menuItem.setToolTipText("About Biocham");
			    menuItem.addActionListener(new ActionListener(){				
					public void actionPerformed(ActionEvent e) {
						BiochamMainFrame.aboutNotOSXPlatform();
						
					}});
			    helpMenu.add(menuItem);
			    bwIcon=null;		    	
			    
		    }    		  
		    menuBar.add(helpMenu);	
    	}
		return menuBar;
    }


    /**
     * 
     * Responds to the events generated by clicking on the menu bar items.
     * In general, it creates popups of all menu item features.
     *      
     */
    public void actionPerformed(ActionEvent e) {
    	
		if(e.getActionCommand().equals("rules")){
			if(((ParamTableRules)BiochamDynamicTree.currentModel.getRules().getParamTable()).getRulesModel().getViews().size()==1){
				BiochamDynamicTree.currentModel.rulesF.getContentPane().removeAll();				
				RulesView v=new RulesView(BiochamDynamicTree.currentModel.rulesF,((ParamTableRules)BiochamDynamicTree.currentModel.getRules().getParamTable()).getRulesModel());					
				JFrame d=BiochamDynamicTree.currentModel.rulesF;
				JScrollPane p=new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
				p.setAutoscrolls(true);
				p.setViewportView(v);
				d.getContentPane().add(p);					
				Point pos = BiochamMainFrame.frame.getLocationOnScreen();
				d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
			    d.setResizable(true);
			    d.setSize(new Dimension(550, 580));	    
			    d.setLocationRelativeTo(BiochamMainFrame.frame);
			    d.setAlwaysOnTop(false);
			    d.setFocusable(true);		    
			    d.setVisible(true);	    
			}else{
				BiochamDynamicTree.currentModel.rulesF.setExtendedState(JFrame.NORMAL);
				BiochamDynamicTree.currentModel.rulesF.setVisible(true);
				BiochamDynamicTree.currentModel.rulesF.setFocusable(true);				
			}		
			
		}else if(e.getActionCommand().equals("molecules")){
			if(((ParamTableMolecules)BiochamDynamicTree.currentModel.getMolecules().getParamTable()).getMoleculesModel().getViews().size()==1){
				BiochamDynamicTree.currentModel.molsF.getContentPane().removeAll();				
				MoleculesView v=new MoleculesView(BiochamDynamicTree.currentModel.molsF,((ParamTableMolecules)BiochamDynamicTree.currentModel.getMolecules().getParamTable()).getMoleculesModel());
				JFrame d=BiochamDynamicTree.currentModel.molsF;
				JScrollPane p=new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
				p.setAutoscrolls(true);
				p.setViewportView(v);
				d.getContentPane().add(p);						
				Point pos = BiochamMainFrame.frame.getLocationOnScreen();
				d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
			    d.setResizable(true);
			    d.setSize(new Dimension(480, 580));	    
			    d.setLocationRelativeTo(BiochamMainFrame.frame);
			    d.setAlwaysOnTop(false);
			    d.setFocusable(true);		    
			    d.setVisible(true);	    
			}else{
				BiochamDynamicTree.currentModel.molsF.setExtendedState(JFrame.NORMAL);
				BiochamDynamicTree.currentModel.molsF.setVisible(true);
				BiochamDynamicTree.currentModel.molsF.setFocusable(true);				
			}
			
			
		}else if(e.getActionCommand().equals("volumes")){
			if(((ParamTableVolumes)BiochamDynamicTree.currentModel.getVolumes().getParamTable()).getVolumesModel().getViews().size()==1){
				BiochamDynamicTree.currentModel.volumeF.getContentPane().removeAll();				
				VolumesView v=new VolumesView(BiochamDynamicTree.currentModel.volumeF,((ParamTableVolumes)BiochamDynamicTree.currentModel.getVolumes().getParamTable()).getVolumesModel());
				JFrame d=BiochamDynamicTree.currentModel.volumeF;
				JScrollPane p=new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
				p.setAutoscrolls(true);
				p.setViewportView(v);
				d.getContentPane().add(p);							
				Point pos = BiochamMainFrame.frame.getLocationOnScreen();
				d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
			    d.setResizable(true);
			    d.setSize(new Dimension(480, 580));	    
			    d.setLocationRelativeTo(BiochamMainFrame.frame);
			    d.setAlwaysOnTop(false);
			    d.setFocusable(true);		    
			    d.setVisible(true);	    
			}else{
				BiochamDynamicTree.currentModel.volumeF.setExtendedState(JFrame.NORMAL);
				BiochamDynamicTree.currentModel.volumeF.setVisible(true);
				BiochamDynamicTree.currentModel.volumeF.setFocusable(true);				
			}
			
			
		}else if(e.getActionCommand().equals("macros")){
			if(((ParamTableMacros)BiochamDynamicTree.currentModel.getMacros().getParamTable()).getMacrosModel().getViews().size()==1){
				BiochamDynamicTree.currentModel.macrosF.getContentPane().removeAll();				
				MacrosView v=new MacrosView(BiochamDynamicTree.currentModel.macrosF,((ParamTableMacros)BiochamDynamicTree.currentModel.getMacros().getParamTable()).getMacrosModel());
				JFrame d=BiochamDynamicTree.currentModel.macrosF;
				JScrollPane p=new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
				p.setAutoscrolls(true);
				p.setViewportView(v);
				d.getContentPane().add(p);							
				Point pos = BiochamMainFrame.frame.getLocationOnScreen();
				d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
			    d.setResizable(true);
			    d.setSize(new Dimension(480, 580));	    
			    d.setLocationRelativeTo(BiochamMainFrame.frame);
			    d.setAlwaysOnTop(false);
			    d.setFocusable(true);		    
			    d.setVisible(true);	    
			}else{
				BiochamDynamicTree.currentModel.macrosF.setExtendedState(JFrame.NORMAL);
				BiochamDynamicTree.currentModel.macrosF.setVisible(true);
				BiochamDynamicTree.currentModel.macrosF.setFocusable(true);				
			}
			
			
		}else if(e.getActionCommand().equals("events")){
			if(((ParamTableEvents)BiochamDynamicTree.currentModel.getEvents().getParamTable()).getEventsModel().getViews().size()==1){
				BiochamDynamicTree.currentModel.eventsF.getContentPane().removeAll();				
				EventsView v=new EventsView(BiochamDynamicTree.currentModel.eventsF,((ParamTableEvents)BiochamDynamicTree.currentModel.getEvents().getParamTable()).getEventsModel());
				JFrame d=BiochamDynamicTree.currentModel.eventsF;
				JScrollPane p=new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
				p.setAutoscrolls(true);
				p.setViewportView(v);
				d.getContentPane().add(p);						
				Point pos = BiochamMainFrame.frame.getLocationOnScreen();
				d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
			    d.setResizable(true);
			    d.setSize(new Dimension(480, 580));	    
			    d.setLocationRelativeTo(BiochamMainFrame.frame);
			    d.setAlwaysOnTop(false);
			    d.setFocusable(true);		    
			    d.setVisible(true);	    
			}else{
				BiochamDynamicTree.currentModel.eventsF.setExtendedState(JFrame.NORMAL);
				BiochamDynamicTree.currentModel.eventsF.setVisible(true);
				BiochamDynamicTree.currentModel.eventsF.setFocusable(true);				
			}
			
					
		}else if(e.getActionCommand().equals("invariants")){
			if(((ParamTableConservationLaws)BiochamDynamicTree.currentModel.getConservationLaws().getParamTable()).getInvariantsModel().getViews().size()==1){
				BiochamDynamicTree.currentModel.conLawsF.getContentPane().removeAll();				
				InvariantsView v=new InvariantsView(BiochamDynamicTree.currentModel.conLawsF,((ParamTableConservationLaws)BiochamDynamicTree.currentModel.getConservationLaws().getParamTable()).getInvariantsModel());
				JFrame d=BiochamDynamicTree.currentModel.conLawsF;
				JScrollPane p=new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
				p.setAutoscrolls(true);
				p.setViewportView(v);
				d.getContentPane().add(p);						
				Point pos = BiochamMainFrame.frame.getLocationOnScreen();
				d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
			    d.setResizable(true);
			    d.setSize(new Dimension(480, 580));	    
			    d.setLocationRelativeTo(BiochamMainFrame.frame);
			    d.setAlwaysOnTop(false);
			    d.setFocusable(true);		    
			    d.setVisible(true);	    
			}else{
				BiochamDynamicTree.currentModel.conLawsF.setVisible(true);
				BiochamDynamicTree.currentModel.conLawsF.setFocusable(true);	
				BiochamDynamicTree.currentModel.conLawsF.setExtendedState(JFrame.NORMAL);
			}			
			
			
		}else if(e.getActionCommand().equals("declarations")){
			if(((ParamTableDeclarations)BiochamDynamicTree.currentModel.getDeclarations().getParamTable()).getDeclarationsModel().getViews().size()==1){
				BiochamDynamicTree.currentModel.declF.getContentPane().removeAll();				
				DeclarationsView v=new DeclarationsView(BiochamDynamicTree.currentModel.declF,((ParamTableDeclarations)BiochamDynamicTree.currentModel.getDeclarations().getParamTable()).getDeclarationsModel());					
				JFrame d=BiochamDynamicTree.currentModel.declF;
				JScrollPane p=new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
				p.setAutoscrolls(true);
				p.setViewportView(v);
				d.getContentPane().add(p);					
				Point pos = BiochamMainFrame.frame.getLocationOnScreen();
				d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
			    d.setResizable(true);
			    d.setSize(new Dimension(480, 580));	    
			    d.setLocationRelativeTo(BiochamMainFrame.frame);
			    d.setAlwaysOnTop(false);
			    d.setFocusable(true);		    
			    d.setVisible(true);	    
			}else{
				BiochamDynamicTree.currentModel.declF.setVisible(true);
				BiochamDynamicTree.currentModel.declF.setFocusable(true);		
				BiochamDynamicTree.currentModel.declF.setExtendedState(JFrame.NORMAL);
			}			
		}else if(e.getActionCommand().equals("parameters")){
			if(((ParamTableParameters)BiochamDynamicTree.currentModel.getParameters().getParamTable()).getParametersModel().getViews().size()==1){
				BiochamDynamicTree.currentModel.paramF.getContentPane().removeAll();				
				ParametersView v=new ParametersView(BiochamDynamicTree.currentModel.paramF,((ParamTableParameters)BiochamDynamicTree.currentModel.getParameters().getParamTable()).getParametersModel());					
				JFrame d=BiochamDynamicTree.currentModel.paramF;
				JScrollPane p=new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
				p.setAutoscrolls(true);
				p.setViewportView(v);
				d.getContentPane().add(p);						
				Point pos = BiochamMainFrame.frame.getLocationOnScreen();
				d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
			    d.setResizable(true);
			    d.setSize(new Dimension(480, 580));	    
			    d.setLocationRelativeTo(BiochamMainFrame.frame);
			    d.setAlwaysOnTop(false);
			    d.setFocusable(true);		    
			    d.setVisible(true);	    
			}else{
				BiochamDynamicTree.currentModel.paramF.setVisible(true);
				BiochamDynamicTree.currentModel.paramF.setFocusable(true);	
				BiochamDynamicTree.currentModel.paramF.setExtendedState(JFrame.NORMAL);
			}			
			
		}else if(e.getActionCommand().equals("initialState")){
			if(((ParamTableInitConc)BiochamDynamicTree.currentModel.getInitConditions().getParamTable()).getInitStateModel().getViews().size()==1){
				BiochamDynamicTree.currentModel.initConcF.getContentPane().removeAll();				
				InitialStateView v=new InitialStateView(BiochamDynamicTree.currentModel.initConcF,((ParamTableInitConc)BiochamDynamicTree.currentModel.getInitConditions().getParamTable()).getInitStateModel());					
				JFrame d=BiochamDynamicTree.currentModel.initConcF;
				JScrollPane p=new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
				p.setAutoscrolls(true);
				p.setViewportView(v);
				d.getContentPane().add(p);						
				Point pos = BiochamMainFrame.frame.getLocationOnScreen();
				d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
			    d.setResizable(true);
			    d.setSize(new Dimension(480, 580));	    
			    d.setLocationRelativeTo(BiochamMainFrame.frame);
			    d.setAlwaysOnTop(false);
			    d.setFocusable(true);		    
			    d.setVisible(true);	    
			}else{
				BiochamDynamicTree.currentModel.initConcF.setVisible(true);
				BiochamDynamicTree.currentModel.initConcF.setFocusable(true);
				//BiochamDynamicTree.currentModel.initConcF.setLocation(BiochamMainFrame.frame.getLocationOnScreen().x+BiochamMainFrame.frame.getSize().width/2-185,BiochamMainFrame.frame.getLocationOnScreen().y+BiochamMainFrame.frame.getSize().height/2-140);
				BiochamDynamicTree.currentModel.initConcF.setExtendedState(JFrame.NORMAL);
								
			}			
			
		}else if(e.getActionCommand().contains("Boolean")){
			if(((ParamTableCTLSpecifications)BiochamDynamicTree.currentModel.getCtlSpecifications().getParamTable()).getCtlModel().getViews().size()==1){
				BiochamDynamicTree.currentModel.booleanTPF.getContentPane().removeAll();				
				CTLView v=new CTLView(BiochamDynamicTree.currentModel.booleanTPF,((ParamTableCTLSpecifications)BiochamDynamicTree.currentModel.getCtlSpecifications().getParamTable()).getCtlModel());					
				JFrame d=BiochamDynamicTree.currentModel.booleanTPF;
				JScrollPane p=new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
				p.setAutoscrolls(true);
				p.setViewportView(v);
				d.getContentPane().add(p);					
				Point pos = BiochamMainFrame.frame.getLocationOnScreen();
				d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
			    d.setResizable(true);
			    d.setSize(new Dimension(480, 580));	    
			    d.setLocationRelativeTo(BiochamMainFrame.frame);
			    d.setAlwaysOnTop(false);
			    d.setFocusable(true);		    
			    d.setVisible(true);	    
			}else{
				BiochamDynamicTree.currentModel.booleanTPF.setExtendedState(JFrame.NORMAL);
				BiochamDynamicTree.currentModel.booleanTPF.setVisible(true);
				BiochamDynamicTree.currentModel.booleanTPF.setFocusable(true);				
			}
			
		}else if(e.getActionCommand().contains("Numerical")){
			if(((ParamTableLTLSpecifications)BiochamDynamicTree.currentModel.getLtlSpecifications().getParamTable()).getLtlModel().getViews().size()==1){
				BiochamDynamicTree.currentModel.numericalTPF.getContentPane().removeAll();				
				LTLView v=new LTLView(BiochamDynamicTree.currentModel.numericalTPF,((ParamTableLTLSpecifications)BiochamDynamicTree.currentModel.getLtlSpecifications().getParamTable()).getLtlModel());					
				JFrame d=BiochamDynamicTree.currentModel.numericalTPF;
				JScrollPane p=new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
				p.setAutoscrolls(true);
				p.setViewportView(v);
				d.getContentPane().add(p);						
				Point pos = BiochamMainFrame.frame.getLocationOnScreen();
				d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
			    d.setResizable(true);
			    d.setSize(new Dimension(480, 580));	    
			    d.setLocationRelativeTo(BiochamMainFrame.frame);
			    d.setAlwaysOnTop(false);
			    d.setFocusable(true);		    
			    d.setVisible(true);	    
			}else{
				BiochamDynamicTree.currentModel.numericalTPF.setExtendedState(JFrame.NORMAL);
				BiochamDynamicTree.currentModel.numericalTPF.setVisible(true);
				BiochamDynamicTree.currentModel.numericalTPF.setFocusable(true);				
			}
		}else if(e.getActionCommand().contains("Abstractions")){
			if(BiochamDynamicTree.currentModel.getAbstractionModel().getViews().size()==1){
				BiochamDynamicTree.currentModel.abstractionsF.getContentPane().removeAll();				
				AbstractionView v=new AbstractionView(BiochamDynamicTree.currentModel.abstractionsF,BiochamDynamicTree.currentModel);					
				JFrame d=BiochamDynamicTree.currentModel.abstractionsF;
				JScrollPane p=new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
				p.setAutoscrolls(true);
				p.setViewportView(v);
				d.getContentPane().add(p);							
				Point pos = BiochamMainFrame.frame.getLocationOnScreen();
				d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
			    d.setResizable(true);
			    d.setSize(new Dimension(480, 580));	    
			    d.setLocationRelativeTo(BiochamMainFrame.frame);
			    d.setAlwaysOnTop(false);
			    d.setFocusable(true);		    
			    d.setVisible(true);	    
			}else{
				BiochamDynamicTree.currentModel.abstractionsF.setExtendedState(JFrame.NORMAL);
				BiochamDynamicTree.currentModel.abstractionsF.setVisible(true);
				BiochamDynamicTree.currentModel.abstractionsF.setFocusable(true);
			}
		}else if(e.getActionCommand().equals("Simulations")){
			if(BiochamDynamicTree.currentModel.getSimulationModel().getViews().size()==1){
				BiochamDynamicTree.currentModel.simulationF.getContentPane().removeAll();
				JFrame d=BiochamDynamicTree.currentModel.simulationF;
				SimulationView v=new SimulationView(d,BiochamDynamicTree.currentModel);			
				JScrollPane p=new JScrollPane(JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS);
				p.setAutoscrolls(true);
				p.setViewportView(v);
				d.getContentPane().add(p);							
				Point pos = BiochamMainFrame.frame.getLocationOnScreen();
				d.setLocation(pos.x+BiochamMainFrame.frame.getSize().width/2-185,pos.y+BiochamMainFrame.frame.getSize().height/2-140);
			    d.setResizable(true);
			    d.setSize(new Dimension(480, 580));	    
			    d.setLocationRelativeTo(BiochamMainFrame.frame);
			    d.setAlwaysOnTop(false);
			    d.setFocusable(true);		    
			    d.setVisible(true);	
			}else{
				BiochamDynamicTree.currentModel.simulationF.setExtendedState(JFrame.NORMAL);
				BiochamDynamicTree.currentModel.simulationF.setVisible(true);
				BiochamDynamicTree.currentModel.simulationF.setFocusable(true);
			}
		}
		
	}
    

    /**
     * 
     * Refreshes the menus context so that the menu Model gets the name of the currently selected Biocham model.
     * 
     */
    public static void refreshMenusContext(BiochamModel m){
    
    	if(m!=null){
    		modelM.setModelName(m.getModelName());
    		for(int i=0;i<menuBar.getMenuCount();i++){
    			menuBar.getMenu(i).setEnabled(true);
    		}
    	}
    }
    

    /**
     *    
     * Enables the menu Model to be accessible if and only if there is at least one Biocham model opened in the GUI.
     * 
     */
    public static void setMenusEnabled(boolean b){
    	 for(int i=0;i<menuBar.getComponentCount();i++){
		    	menuBar.getComponent(i).setEnabled(b);
    	 }
    	 menuBar.revalidate();
    }


    /**
     * 
     * Utility method that creates an instance of a menuItem such that has an icon, name, key, and applies to it common visual properties like font, background and foreground.
     * 
     */
	public static JMenuItem createMenuItem(String name, int key, ImageIcon imageIcon) {
		
			
		JMenuItem menuItem=new JMenuItem(name,imageIcon);
		menuItem.setMnemonic(key);		
		menuItem.setAccelerator(KeyStroke.getKeyStroke(key,Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()));
		menuItem.setBackground(Utils.backgroundColor);
		menuItem.setFont(Utils.menuBarFont);
		menuItem.setForeground(Utils.foregroundColor);		
		return menuItem;  
	}


	 /**
     * 
     * Utility method that creates an instance of a simple colored menuItem with a given title.
     * 
     */
	public static ColorMenu createNewMenu(String string) {
		
		ColorMenu menu2 = new ColorMenu(string);
		return menu2;
	}
	/**
     * 
     * Utility method that creates an instance of a colored menuItem with a given name and key.
     * 
     */
	public static ColorMenu createNewMenu(String string,int key) {
		
		ColorMenu menu2 = new ColorMenu(string);	
		menu2.setMnemonic(key);
		
		return menu2;
	}
	
	/**
     * 
     * Utility method that creates an instance of a popup menu with applied common visual properties like background and foreground color and font.
     * 
     */
	public static JPopupMenu createNewPopupMenu() {
		
		JPopupMenu menu = new JPopupMenu(); 
		menu.setFont(Utils.treeExplorerFont);
		menu.setForeground(Utils.foregroundColor);
		menu.setBackground(Utils.backgroundColor);
		
		
		return menu;
	}	
}

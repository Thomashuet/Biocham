package fr.inria.contraintes.biocham.menus;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.modelData.ParamTableLTLSpecifications;
import fr.inria.contraintes.biocham.utils.Utils;

import java.awt.event.ActionListener;

import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

public class ElementMenuNumTemporal extends ElementMenu{

	ColorMenu menu;
	JPopupMenu pMenu;
	ColorMenu computeMenu, searchMenu;
	ColorMenu pcomputeMenu, psearchMenu;
	JMenuItem s3MenuItem,s4MenuItem,s5MenuItem,s6MenuItem,clearAllMenuItem;
	JMenuItem ps3MenuItem,ps4MenuItem,ps5MenuItem,ps6MenuItem,pclearAllMenuItem;
	JMenuItem checkMenuItem,pcheckMenuItem,addMenuItem,paddMenuItem,loadTraceMenuItem,ploadTraceMenuItem,
			  satifDegree,psatifDegree,robustDegree,probustDegree,satifLandMenuItem,psatifLandMenuItem,s1MenuItem,ps1MenuItem,
			  s2MenuItem,ps2MenuItem;
	JMenuItem[] items;
	BiochamDynamicTree tree;
	ActionListener al;
	public ElementMenuNumTemporal(BiochamDynamicTree t){		
		tree=t;		
		createMenu();
		createPopupMenu();
		items=new JMenuItem[]{checkMenuItem,pcheckMenuItem,addMenuItem,paddMenuItem,loadTraceMenuItem,ploadTraceMenuItem,
				  satifDegree,psatifDegree,robustDegree,probustDegree,satifLandMenuItem,psatifLandMenuItem,s1MenuItem,ps1MenuItem,
				  s2MenuItem,ps2MenuItem,ps3MenuItem,ps4MenuItem,ps5MenuItem,ps6MenuItem,pclearAllMenuItem,s3MenuItem,s4MenuItem,
				  s5MenuItem,s6MenuItem,clearAllMenuItem,computeMenu, searchMenu,pcomputeMenu, psearchMenu};
	}
	
	
	private void createMenu(){
			
		
		 menu=BiochamMenuBar.createNewMenu("LTLTemporalProperties", 0);		
		 checkMenuItem = newMenuItem("Check Property"); 
		 checkMenuItem.setActionCommand("traceAnalyze");

		 addMenuItem = newMenuItem("Add Property"); 
		 addMenuItem.setActionCommand("addLTL");
		
		 
		 loadTraceMenuItem = newMenuItem("Load Trace"); 
		 loadTraceMenuItem.setActionCommand("loadTrace");
		
		 
		 computeMenu = newMenu("Compute"); 
		
		 satifDegree=newMenuItem("Satisfaction Degree");
		 satifDegree.setActionCommand("satisfactionDegree");
	
		 computeMenu.add(satifDegree);
		 robustDegree=newMenuItem("Robustness");
		 robustDegree.setActionCommand("robustness");
;
		 computeMenu.add(robustDegree);
		 computeMenu.setEnabled(true);
		 computeMenu.setBackground(Utils.backgroundColor);
		 computeMenu.setForeground(Utils.foregroundColor);	    
		 computeMenu.setFont(Utils.treeExplorerFont);
		 computeMenu.revalidate();
		 computeMenu.setOpaque(true);
		 computeMenu.setVisible(true);
		 
		 satifLandMenuItem = newMenuItem("Satisfaction Landscape"); 
		 satifLandMenuItem.setActionCommand("landscape");

		 
		 searchMenu = newMenu("Search Parameters"); 
		
		 s1MenuItem=newMenuItem("CMAES");
		 s1MenuItem.setActionCommand("search_parameters_cmaes");
	
		 searchMenu.add(s1MenuItem);
		 s2MenuItem=newMenuItem("CMAES Multi-Conditions");
		 s2MenuItem.setActionCommand("cmaes_multi_conditions");
	
		 searchMenu.add(s2MenuItem);
		 
		 s3MenuItem=newMenuItem("Max 2Parameters");
		 s3MenuItem.setActionCommand("search_parameters");
	
		 s3MenuItem.setEnabled(false);
		 searchMenu.add(s3MenuItem);
		 s4MenuItem=newMenuItem("Max 2Parameters-ALL");
		 s4MenuItem.setActionCommand("search_all_parameters");

		 s4MenuItem.setEnabled(false);
		 searchMenu.add(s4MenuItem);
		 s5MenuItem=newMenuItem("Random");
		 s5MenuItem.setActionCommand("search_random_parameters");

		 s5MenuItem.setEnabled(false);
		 searchMenu.add(s5MenuItem);
		 s6MenuItem=newMenuItem("Random ALL");
		 s6MenuItem.setActionCommand("search_random_all_parameters");
	
		 s6MenuItem.setEnabled(false);
		 searchMenu.add(s6MenuItem);
		 
		 
		 searchMenu.setEnabled(true);
		 searchMenu.setBackground(Utils.backgroundColor);
		 searchMenu.setForeground(Utils.foregroundColor);	    
		 searchMenu.setFont(Utils.treeExplorerFont);
		 searchMenu.revalidate();
		 searchMenu.setOpaque(true);
		 searchMenu.setVisible(true);
		 
		 
		 clearAllMenuItem = newMenuItem("Delete All"); 
		 clearAllMenuItem.setActionCommand("deleteParameters");
	
		 
	    	menu.add(checkMenuItem);
	    	menu.add(addMenuItem);
	    	menu.addSeparator();
	    	menu.add(loadTraceMenuItem);
	    	menu.addSeparator();
	    	menu.add(computeMenu);
	    //	menu.addSeparator();
	    	menu.add(satifLandMenuItem);
	    	//menu.addSeparator();
	    	menu.add(searchMenu);
	    	menu.addSeparator();
	    	
	    	menu.add(clearAllMenuItem);
	    	
		this.setMenu(menu);
	}
	
	
	private void createPopupMenu(){
	
		
		 pMenu=BiochamMenuBar.createNewPopupMenu();
		
		 pcheckMenuItem = newMenuItem("Check Property"); 
		 pcheckMenuItem.setActionCommand("traceAnalyze");

		 paddMenuItem = newMenuItem("Add Property"); 
		 paddMenuItem.setActionCommand("addLTL");
	
		 
		 ploadTraceMenuItem = newMenuItem("Load Trace"); 
		 ploadTraceMenuItem.setActionCommand("loadTrace");

		 
		 pcomputeMenu = newMenu("Compute"); 
		
		 psatifDegree=newMenuItem("Satisfaction Degree");
		 psatifDegree.setActionCommand("satisfactionDegree");

		 pcomputeMenu.add(psatifDegree);
		 probustDegree=newMenuItem("Robustness");
		 probustDegree.setActionCommand("robustness");
		
		 pcomputeMenu.add(probustDegree);
		 pcomputeMenu.setEnabled(true);
		 pcomputeMenu.setBackground(Utils.backgroundColor);
		 pcomputeMenu.setForeground(Utils.foregroundColor);	    
		 pcomputeMenu.setFont(Utils.treeExplorerFont);
		 pcomputeMenu.revalidate();
		 pcomputeMenu.setOpaque(true);
		 pcomputeMenu.setVisible(true);
		 
		 psatifLandMenuItem = newMenuItem("Satisfaction Landscape"); 
		 psatifLandMenuItem.setActionCommand("landscape");
		
		 
		 psearchMenu = newMenu("Search Parameters"); 
		
		 ps1MenuItem=newMenuItem("CMAES");
		 ps1MenuItem.setActionCommand("search_parameters_cmaes");
	
		 psearchMenu.add(ps1MenuItem);
		 ps2MenuItem=newMenuItem("CMAES Multi-Conditions");
		 ps2MenuItem.setActionCommand("cmaes_multi_conditions");
		
		 psearchMenu.add(ps2MenuItem);
		 
		 ps3MenuItem=newMenuItem("Max 2Parameters");
		 ps3MenuItem.setActionCommand("search_parameters");

		 ps3MenuItem.setEnabled(false);
		 psearchMenu.add(ps3MenuItem);
		 ps4MenuItem=newMenuItem("Max 2Parameters-ALL");
		 ps4MenuItem.setActionCommand("search_all_parameters");
	
		 ps4MenuItem.setEnabled(false);
		 psearchMenu.add(ps4MenuItem);
		 ps5MenuItem=newMenuItem("Random");
		 ps5MenuItem.setActionCommand("search_random_parameters");
		
		 ps5MenuItem.setEnabled(false);
		 psearchMenu.add(ps5MenuItem);
		 ps6MenuItem=newMenuItem("Random ALL");
		 ps6MenuItem.setActionCommand("search_random_all_parameters");
	
		 ps6MenuItem.setEnabled(false);
		 psearchMenu.add(ps6MenuItem);
		 
		 
		 psearchMenu.setEnabled(true);
		 psearchMenu.setBackground(Utils.backgroundColor);
		 psearchMenu.setForeground(Utils.foregroundColor);	    
		 psearchMenu.setFont(Utils.treeExplorerFont);
		 psearchMenu.revalidate();
		 psearchMenu.setOpaque(true);
		 psearchMenu.setVisible(true);
		 
		 
		 pclearAllMenuItem = newMenuItem("Delete All"); 
		 pclearAllMenuItem.setActionCommand("deleteParameters");
	
		 pMenu.add(checkMenuItem);
	    	pMenu.add(paddMenuItem);
	    	pMenu.addSeparator();
	    	pMenu.add(ploadTraceMenuItem);
	    	pMenu.addSeparator();
	    	pMenu.add(pcomputeMenu);
	    	//pMenu.addSeparator();
	    	pMenu.add(psatifLandMenuItem);
	    	//pMenu.addSeparator();
	    	pMenu.add(psearchMenu);
	    	pMenu.addSeparator();
	    	pMenu.add(pclearAllMenuItem);
		 this.setPMenu(pMenu);
		
	}
	
	@Override
	public void setMenu(ColorMenu m) {
		menu=m;		
	}
	@Override
	public void setPMenu(JPopupMenu m) {
		pMenu=m;		
	}
	
	public ColorMenu getMenu(){
		return menu;
	}
	public JPopupMenu getPMenu(){
		return pMenu;
	}


	@Override
	public ColorMenu refreshed(BiochamModel m) {
		
		al=(((ParamTableLTLSpecifications)m.getLtlSpecifications().getParamTable()));
		
		for(int i=0;i<items.length;i++){
			
			if(items[i].getActionListeners().length>0){
				 for(int j=0;j<items[i].getActionListeners().length;j++){
					 items[i].removeActionListener(items[i].getActionListeners()[j]);
				 }
			 }
			items[i].addActionListener(al);		
		}
			
		
		/*al=(ParamTableInitConc)m.getInitConditions().getParamTable();

		if(checkMenuItem.getActionListeners().length>0){
			 for(int i=0;i<checkMenuItem.getActionListeners().length;i++){
				 checkMenuItem.removeActionListener(checkMenuItem.getActionListeners()[i]);
			 }
		 }
		checkMenuItem.addActionListener(al);
		if(pcheckMenuItem.getActionListeners().length>0){
			 for(int i=0;i<pcheckMenuItem.getActionListeners().length;i++){
				 pcheckMenuItem.removeActionListener(pcheckMenuItem.getActionListeners()[i]);
			 }
		 }
		pcheckMenuItem.addActionListener(al);
		
		if(loadTraceMenuItem.getActionListeners().length>0){
			 for(int i=0;i<loadTraceMenuItem.getActionListeners().length;i++){
				 loadTraceMenuItem.removeActionListener(loadTraceMenuItem.getActionListeners()[i]);
			 }
		 }
		loadTraceMenuItem.addActionListener(al);
		if(ploadTraceMenuItem.getActionListeners().length>0){
			 for(int i=0;i<ploadTraceMenuItem.getActionListeners().length;i++){
				 ploadTraceMenuItem.removeActionListener(ploadTraceMenuItem.getActionListeners()[i]);
			 }
		 }
		ploadTraceMenuItem.addActionListener(al);
		
		if(satifDegree.getActionListeners().length>0){
			 for(int i=0;i<satifDegree.getActionListeners().length;i++){
				 satifDegree.removeActionListener(satifDegree.getActionListeners()[i]);
			 }
		 }
		satifDegree.addActionListener(al);
		if(psatifDegree.getActionListeners().length>0){
			 for(int i=0;i<psatifDegree.getActionListeners().length;i++){
				 psatifDegree.removeActionListener(psatifDegree.getActionListeners()[i]);
			 }
		 }
		psatifDegree.addActionListener(al);
		
		if(robustDegree.getActionListeners().length>0){
			 for(int i=0;i<robustDegree.getActionListeners().length;i++){
				 robustDegree.removeActionListener(robustDegree.getActionListeners()[i]);
			 }
		 }
		robustDegree.addActionListener(al);
		if(probustDegree.getActionListeners().length>0){
			 for(int i=0;i<probustDegree.getActionListeners().length;i++){
				 probustDegree.removeActionListener(probustDegree.getActionListeners()[i]);
			 }
		 }
		probustDegree.addActionListener(al);
		
		if(satifLandMenuItem.getActionListeners().length>0){
			 for(int i=0;i<satifLandMenuItem.getActionListeners().length;i++){
				 satifLandMenuItem.removeActionListener(satifLandMenuItem.getActionListeners()[i]);
			 }
		 }
		satifLandMenuItem.addActionListener(al);
		if(psatifLandMenuItem.getActionListeners().length>0){
			 for(int i=0;i<psatifLandMenuItem.getActionListeners().length;i++){
				 psatifLandMenuItem.removeActionListener(psatifLandMenuItem.getActionListeners()[i]);
			 }
		 }
		psatifLandMenuItem.addActionListener(al);
		
		if(s1MenuItem.getActionListeners().length>0){
			 for(int i=0;i<s1MenuItem.getActionListeners().length;i++){
				 s1MenuItem.removeActionListener(s1MenuItem.getActionListeners()[i]);
			 }
		 }
		s1MenuItem.addActionListener(al);
		if(s1MenuItem.getActionListeners().length>0){
			 for(int i=0;i<s1MenuItem.getActionListeners().length;i++){
				 s1MenuItem.removeActionListener(s1MenuItem.getActionListeners()[i]);
			 }
		 }
		s1MenuItem.addActionListener(al);
		*/
		if(((ParamTableLTLSpecifications)m.getLtlSpecifications().getParamTable()).getLtlModel().getSpecifications().size()>0){
			s3MenuItem.setEnabled(true);
			s4MenuItem.setEnabled(true);
			s5MenuItem.setEnabled(true);
			s6MenuItem.setEnabled(true);			
			ps3MenuItem.setEnabled(true);
			ps4MenuItem.setEnabled(true);
			ps5MenuItem.setEnabled(true);
			ps6MenuItem.setEnabled(true);
			pclearAllMenuItem.setEnabled(true);
			clearAllMenuItem.setEnabled(true);
		}else{
			s3MenuItem.setEnabled(false);
			s4MenuItem.setEnabled(false);
			s5MenuItem.setEnabled(false);
			s6MenuItem.setEnabled(false);
			ps3MenuItem.setEnabled(false);
			ps4MenuItem.setEnabled(false);
			ps5MenuItem.setEnabled(false);
			ps6MenuItem.setEnabled(false);
			pclearAllMenuItem.setEnabled(false);
			clearAllMenuItem.setEnabled(false);
		}
		return this.menu;
	}


	@Override
	public JPopupMenu refreshedPopup(BiochamModel m) {
		// TODO Auto-generated method stub
		return null;
	}
}


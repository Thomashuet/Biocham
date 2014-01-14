package fr.inria.contraintes.biocham.menus;

import fr.inria.contraintes.biocham.BiochamDynamicTree;
import fr.inria.contraintes.biocham.BiochamModel;
import fr.inria.contraintes.biocham.modelData.ParamTableCTLSpecifications;

import java.awt.event.ActionListener;

import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

public class ElementMenuBoolTemporal extends ElementMenu{

	ColorMenu menu;
	JPopupMenu pMenu;
	JMenuItem reduce,learn,revise,clearAllMenuItem;
	JMenuItem preduce,plearn,previse,pclearAllMenuItem;	
	JMenuItem checkMenuItem,pcheckMenuItem,addMenuItem,paddMenuItem,generateMenuItem,pgenerateMenuItem,
			  addGeneratedMenuItem,paddGeneratedMenuItem;
	
	BiochamDynamicTree tree;
	ActionListener al;
	public ElementMenuBoolTemporal(BiochamDynamicTree t){		
		tree=t;
		
		createMenu();
		createPopupMenu();
		
	}
	
	
	private void createMenu(){
			
		
		 menu=BiochamMenuBar.createNewMenu("CTLTemporalProperties", 0);		
		 checkMenuItem = newMenuItem("Check Property"); 
		 checkMenuItem.setActionCommand("nusmv");
	
		 addMenuItem = newMenuItem("Add Property"); 
		 addMenuItem.setActionCommand("addCTL");

		 		
		 generateMenuItem = newMenuItem("Generate Properties"); 
		 generateMenuItem.setActionCommand("genCTL");
		
		 addGeneratedMenuItem = newMenuItem("Add Generated Properties"); 
		 addGeneratedMenuItem.setActionCommand("addGenCTL");

		 
		 reduce = newMenuItem("Reduce Model"); 
		 reduce.setActionCommand("reduceModel");
	
		 learn = newMenuItem("Learn Rules"); 
		 learn.setActionCommand("learnRules");
		 
		 revise = newMenuItem("Revise Model"); 
		 revise.setActionCommand("reviseModel");
		 
		 
		 clearAllMenuItem = newMenuItem("Delete All"); 
		 clearAllMenuItem.setActionCommand("deleteParameters");
		
		 menu.add(checkMenuItem);
		 menu.add(addMenuItem);
		 menu.addSeparator();
		 menu.add(generateMenuItem);	    	
		 menu.add(addGeneratedMenuItem);
		 menu.addSeparator();
		 menu.add(reduce);
		 menu.add(learn);
		 menu.add(revise);
		 menu.addSeparator();
		 menu.add(clearAllMenuItem);
	    	
		this.setMenu(menu);
	}
	
	
	private void createPopupMenu(){
	
		
		pMenu=BiochamMenuBar.createNewPopupMenu();
		
		 pcheckMenuItem = newMenuItem("Check Property"); 
		 pcheckMenuItem.setActionCommand("nusmv");
		
		 paddMenuItem = newMenuItem("Add Property"); 
		 paddMenuItem.setActionCommand("addCTL");
		
		 		
		 pgenerateMenuItem = newMenuItem("Generate Properties"); 
		 pgenerateMenuItem.setActionCommand("genCTL");

		 paddGeneratedMenuItem = newMenuItem("Add Generated Properties"); 
		 paddGeneratedMenuItem.setActionCommand("addGenCTL");
		
		 
		 preduce = newMenuItem("Reduce Model"); 
		 preduce.setActionCommand("reduceModel");

		 plearn = newMenuItem("Learn Rules"); 
		 plearn.setActionCommand("learnRules");
	
		 previse = newMenuItem("Revise Model"); 
		 previse.setActionCommand("reviseModel");

		 
		 pclearAllMenuItem = newMenuItem("Delete All"); 
		 pclearAllMenuItem.setActionCommand("deleteParameters");
	
		 pMenu.add(pcheckMenuItem);
		 pMenu.add(paddMenuItem);
		 pMenu.addSeparator();
		 pMenu.add(pgenerateMenuItem);	    	
		 pMenu.add(paddGeneratedMenuItem);
		 pMenu.addSeparator();
		 pMenu.add(preduce);
		 pMenu.add(plearn);
		 pMenu.add(previse);
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
		//menu=(JMnu)refreshed(tree.currentModel);
		return menu;
	}
	public JPopupMenu getPMenu(){
		return pMenu;
	}


	@Override
	public ColorMenu refreshed(BiochamModel m) {	
		al=(((ParamTableCTLSpecifications)m.getCtlSpecifications().getParamTable()));
		
		if(addMenuItem.getActionListeners().length>0){
			 for(int i=0;i<addMenuItem.getActionListeners().length;i++){
				 addMenuItem.removeActionListener(addMenuItem.getActionListeners()[i]);
			 }
		 }
		addMenuItem.addActionListener(al);
		if(paddMenuItem.getActionListeners().length>0){
			 for(int i=0;i<paddMenuItem.getActionListeners().length;i++){
				 paddMenuItem.removeActionListener(paddMenuItem.getActionListeners()[i]);
			 }
		 }
		paddMenuItem.addActionListener(al);
	
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
		
		if(generateMenuItem.getActionListeners().length>0){
			 for(int i=0;i<generateMenuItem.getActionListeners().length;i++){
				 generateMenuItem.removeActionListener(generateMenuItem.getActionListeners()[i]);
			 }
		 }
		generateMenuItem.addActionListener(al);
		if(pgenerateMenuItem.getActionListeners().length>0){
			 for(int i=0;i<pgenerateMenuItem.getActionListeners().length;i++){
				 pgenerateMenuItem.removeActionListener(pgenerateMenuItem.getActionListeners()[i]);
			 }
		 }
		pgenerateMenuItem.addActionListener(al);
		
		if(addGeneratedMenuItem.getActionListeners().length>0){
			 for(int i=0;i<addGeneratedMenuItem.getActionListeners().length;i++){
				 addGeneratedMenuItem.removeActionListener(addGeneratedMenuItem.getActionListeners()[i]);
			 }
		 }
		addGeneratedMenuItem.addActionListener(al);
		if(paddGeneratedMenuItem.getActionListeners().length>0){
			 for(int i=0;i<paddGeneratedMenuItem.getActionListeners().length;i++){
				 paddGeneratedMenuItem.removeActionListener(paddGeneratedMenuItem.getActionListeners()[i]);
			 }
		 }
		paddGeneratedMenuItem.addActionListener(al);
		
		//reduce
		if(reduce.getActionListeners().length>0){
			 for(int i=0;i<reduce.getActionListeners().length;i++){
				 reduce.removeActionListener(reduce.getActionListeners()[i]);
			 }
		 }
		reduce.addActionListener(al);
		if(preduce.getActionListeners().length>0){
			 for(int i=0;i<preduce.getActionListeners().length;i++){
				 preduce.removeActionListener(preduce.getActionListeners()[i]);
			 }
		 }
		preduce.addActionListener(al);
		
		//revise
		if(revise.getActionListeners().length>0){
			 for(int i=0;i<revise.getActionListeners().length;i++){
				 revise.removeActionListener(revise.getActionListeners()[i]);
			 }
		 }
		revise.addActionListener(al);
		if(previse.getActionListeners().length>0){
			 for(int i=0;i<previse.getActionListeners().length;i++){
				 previse.removeActionListener(previse.getActionListeners()[i]);
			 }
		 }
		previse.addActionListener(al);
		
		//learn
		if(learn.getActionListeners().length>0){
			 for(int i=0;i<learn.getActionListeners().length;i++){
				 learn.removeActionListener(learn.getActionListeners()[i]);
			 }
		 }
		learn.addActionListener(al);
		if(plearn.getActionListeners().length>0){
			 for(int i=0;i<plearn.getActionListeners().length;i++){
				 plearn.removeActionListener(plearn.getActionListeners()[i]);
			 }
		 }
		plearn.addActionListener(al);
		//clearAllMenuItem
		if(clearAllMenuItem.getActionListeners().length>0){
			 for(int i=0;i<clearAllMenuItem.getActionListeners().length;i++){
				 clearAllMenuItem.removeActionListener(clearAllMenuItem.getActionListeners()[i]);
			 }
		 }
		clearAllMenuItem.addActionListener(al);
		if(pclearAllMenuItem.getActionListeners().length>0){
			 for(int i=0;i<pclearAllMenuItem.getActionListeners().length;i++){
				 pclearAllMenuItem.removeActionListener(pclearAllMenuItem.getActionListeners()[i]);
			 }
		 }
		pclearAllMenuItem.addActionListener(al);
		
		if(((ParamTableCTLSpecifications)m.getCtlSpecifications().getParamTable()).getCtlModel().getSpecifications().size()>0){
			reduce.setEnabled(true);
			learn.setEnabled(true);
			revise.setEnabled(true);
			clearAllMenuItem.setEnabled(true);	
			preduce.setEnabled(true);
			plearn.setEnabled(true);
			previse.setEnabled(true);
			pclearAllMenuItem.setEnabled(true);	
		}else{
			reduce.setEnabled(false);
			learn.setEnabled(false);
			revise.setEnabled(false);
			clearAllMenuItem.setEnabled(false);
			preduce.setEnabled(false);
			plearn.setEnabled(false);
			previse.setEnabled(false);
			pclearAllMenuItem.setEnabled(false);
		}
		return this.menu;
	}


	@Override
	public JPopupMenu refreshedPopup(BiochamModel m) {
		// TODO Auto-generated method stub
		return null;
	}
}



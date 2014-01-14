package fr.inria.contraintes.biocham;


import fr.inria.contraintes.biocham.customComponents.WarningsPane;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextPane;
import javax.swing.SwingConstants;
import javax.swing.text.AttributeSet;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;
import javax.swing.text.StyleContext;
import javax.swing.tree.DefaultMutableTreeNode;





/**
 * Class that holds the warnings and errors produced during the work with a biocham model.
 * 
 * @author Dragana Jovanovska  
 */ 
public class BiochamWarnings {
	
	DefaultMutableTreeNode parent;
	BiochamModel model;	
	WarningsPane tarea;
	JPanel panel;
	public BiochamWarnings(DefaultMutableTreeNode p,BiochamModel m){
		parent=p;
		model=m;
		tarea = new WarningsPane();
		panel=new JPanel(new BorderLayout());
		panel.setBackground(Color.white);
		panel.add(tarea,BorderLayout.CENTER);
		m.setWarningsPanel(panel);		
	
		WorkbenchArea.tree.addWarningsNodeToTree(p,this);
	}
	public BiochamWarnings(){}
	
	public BiochamModel getModel() {
		return model;
	}
	public void setModel(BiochamModel model) {
		this.model = model;
	}
	public String toString(){
		return "BiochamWarnings";
	}
	public WarningsPane getTarea() {
		tarea.setEditable(false);	
		return tarea;
	}
	
	public void append(WarningsPane tarea,Color c,String text){
		
		StyleContext sc=StyleContext.getDefaultStyleContext();
		AttributeSet aSet=sc.addAttribute(SimpleAttributeSet.EMPTY, StyleConstants.Foreground,c);
		int len=tarea.getDocument().getLength();
		tarea.setCaretPosition(len);
		tarea.setCharacterAttributes(aSet, false);
		tarea.replaceSelection(text);
	}
	
	public void append(Color c,String text){
				
		tarea.setEditable(true);	
		StyleContext sc=StyleContext.getDefaultStyleContext();
		AttributeSet aSet=sc.addAttribute(SimpleAttributeSet.EMPTY, StyleConstants.Foreground,c);
		int len=tarea.getDocument().getLength();
		tarea.setCaretPosition(len);
		tarea.setCharacterAttributes(aSet, false);
		tarea.replaceSelection(text);
		model.setModelWarnings(this);
	}
	public void disposeElement() {
		parent=null;
		model=null;	
		tarea=null;
		
	}
	
}

package fr.inria.contraintes.biocham.customComponents;

import java.awt.Dimension;

import net.java.balloontip.BalloonTip;

import javax.swing.JComboBox;

/**
 * Class that creates a custom JComboBox component that has nothing new but justifies its length up to the biggest length of the items included in his list.
 * 
 * @author Dragana Jovanovska  
 */ 
public class SimpleComboBox extends JComboBox{

	
	BalloonTip bTip;
	 
	 
	public SimpleComboBox(){
		super();
	}
	
	public SimpleComboBox(String[] list){
		super(list);
		Dimension d=this.getPreferredSize();
		int maxWidth=d.width;
		for(int i=0;i<list.length;i++){
			if(list[i].length()>maxWidth){
				maxWidth=list[i].length();
			}
		}
		setPreferredSize(new Dimension(maxWidth+20, d.height));		
	}

	public BalloonTip getBTip() {
		return bTip;
	}

	public void setBTip(BalloonTip tip) {
		bTip = tip;
	}
	
	public void removebTip(){
		bTip.setEnabled(false);
    	bTip.setVisible(false);
	}
	 
	 
}

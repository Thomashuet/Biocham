package fr.inria.contraintes.biocham.customComponents;

import javax.swing.JPanel;

import fr.inria.contraintes.biocham.utils.Utils;

public class ScrollingPanel extends UniversalScrollPane{

	public ScrollingPanel(JPanel p) {
		super(p);
		setBackground(Utils.backgroundColor);
		setSize(p.getSize());
		// TODO Auto-generated constructor stub
	}

}

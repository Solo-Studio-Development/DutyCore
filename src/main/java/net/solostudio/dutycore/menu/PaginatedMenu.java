package net.solostudio.dutycore.menu;

import net.solostudio.dutycore.managers.MenuController;
import org.jetbrains.annotations.NotNull;

public abstract class PaginatedMenu extends Menu {
    protected int page = 0;
    public abstract int getMaxItemsPerPage();

    public abstract void addMenuBorder();

    public PaginatedMenu(@NotNull MenuController menuController) {
        super(menuController);
    }
}

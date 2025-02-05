package net.solostudio.dutycore.menu;

import lombok.Getter;
import net.solostudio.dutycore.enums.keys.ItemKeys;
import net.solostudio.dutycore.data.MenuController;
import org.jetbrains.annotations.NotNull;

public abstract class PaginatedMenu extends Menu {

    @Getter
    protected int maxItemsPerPage = getSlots() - 3;
    protected int page = 0;

    public PaginatedMenu(@NotNull MenuController menuController) {
        super(menuController);
    }

    public void addMenuBorder() {
        inventory.setItem(ItemKeys.BACK_ITEM.getSlot(), ItemKeys.BACK_ITEM.getItem());
        inventory.setItem(ItemKeys.FORWARD_ITEM.getSlot(), ItemKeys.FORWARD_ITEM.getItem());
        inventory.setItem(ItemKeys.MAIN_ITEM.getSlot(), ItemKeys.MAIN_ITEM.getItem());
    }
}

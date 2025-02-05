package net.solostudio.dutycore.enums.keys;

import net.solostudio.dutycore.item.ItemBuilder;
import net.solostudio.dutycore.item.ItemFactory;
import org.bukkit.Material;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

public enum ItemKeys {
    FILLER_GLASS("filler-glass-item"),

    NAVIGATION_MENU_STAFF("navigation-menu.staff-menu-item"),

    STAFF_ITEM("staff-menu.staff-item"),

    BACK_ITEM("back-item"),
    FORWARD_ITEM("forward-item"),
    MAIN_ITEM("main-item");

    private final String path;

    ItemKeys(@NotNull final String path) {
        this.path = path;
    }

    public ItemStack getItem() {
        return ItemFactory.createItemFromString(path).orElse(new ItemStack(Material.AIR));
    }

    public void getItem(@NotNull Inventory inventory) {
        ItemFactory.createItemFromString(path, inventory);
    }

    public int getSlot() {
        return ItemFactory.getItemSlotFromString(path);
    }
}

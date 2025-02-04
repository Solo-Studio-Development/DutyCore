package net.solostudio.dutycore.enums.keys;

import net.solostudio.dutycore.item.ItemFactory;
import org.bukkit.inventory.ItemStack;
import org.jetbrains.annotations.NotNull;

public enum ItemKeys {
    FILLER_GLASS("filler-glass-item"),

    NAVIGATION_MENU_STAFF("navigation-menu.staff-menu-item"),
    NAVIGATION_MENU_SANCTION("navigation-menu.sanction-menu-item"),

    STAFF_ITEM("staff-menu.staff-item"),

    SANCTION_MENU_WARN("sanction-menu.warn-item"),
    SANCTION_MENU_KICK("sanction-menu.kick-item"),
    SANCTION_MENU_MUTE("sanction-menu.mute-item"),
    SANCTION_MENU_BAN("sanction-menu.ban-item");

    private final String path;

    ItemKeys(@NotNull final String path) {
        this.path = path;
    }

    public ItemStack getItem() {
        return ItemFactory.createItemFromString(path);
    }
}

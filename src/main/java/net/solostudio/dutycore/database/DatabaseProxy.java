package net.solostudio.dutycore.database;

import net.solostudio.dutycore.DutyCore;
import net.solostudio.dutycore.utils.LoggerUtils;
import org.jetbrains.annotations.NotNull;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;

public class DatabaseProxy {
    @SuppressWarnings("unchecked")
    public static <T> T createProxy(@NotNull Class<T> clazz, T instance) {
        return (T) Proxy.newProxyInstance(
                clazz.getClassLoader(),
                new Class<?>[]{clazz},
                new DatabaseInvocationHandler(instance)
        );
    }

    private record DatabaseInvocationHandler(@NotNull Object instance) implements InvocationHandler {
        @Override
        public Object invoke(@NotNull Object proxy, @NotNull Method method, @NotNull Object[] args) throws Throwable {
            if (method.getReturnType().equals(Void.TYPE)) {

                switch (method.getName()) {
                    case "joinDuty",
                         "leaveDuty",
                         "setStaffRank",
                         "setPlayerRank" -> {
                        return method.invoke(instance, args);
                    }
                }

                DutyCore.getInstance().getScheduler().runTaskAsynchronously(() -> {
                    try {
                        method.invoke(instance, args);
                    } catch (Exception ignored) {
                        LoggerUtils.error("An error occurred while invoking a method asynchronously.");
                    }
                });
                return null;
            }

            return method.invoke(instance, args);
        }
    }
}

public class problem2 {
    public static void main(String[] args) {
        System.out.println("Starting...");
        int evenSum = 0;
        int limit = 120;
        int first = 1;
        int second = 1;
        for (int i = 0; i < limit; i++) {
            int third = second + first;
            if (third > 4000000)
                break;
            first = second;
            second = third;
            if (third % 2 == 0) {
                evenSum += third;
            }
        }
        System.out.println("The sum of even fibs till " + limit + " is " + evenSum);
    }
}

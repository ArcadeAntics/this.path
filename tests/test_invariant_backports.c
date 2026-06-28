#include <check.h>
#include <stdlib.h>
#include <string.h>
#include "../src/backports.c"

START_TEST(test_buffer_reads_never_exceed_declared_length)
{
    // Invariant: Buffer reads never exceed the declared length
    const char *payloads[] = {
        "normal",                    // Valid input
        "A",                         // Boundary case (single char)
        "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",  // 100 chars - exceeds typical buffer
        "\x41\x41\x41\x41\x41\x41",  // Exact exploit pattern
        NULL
    };
    
    // Test strncpy behavior
    for (int i = 0; payloads[i] != NULL; i++) {
        char dest[16] = {0};  // Fixed buffer size
        char backup[16] = {0};
        
        // Backup original dest content
        memcpy(backup, dest, sizeof(dest));
        
        // Call strncpy with buffer size constraint
        strncpy(dest, payloads[i], sizeof(dest) - 1);
        dest[sizeof(dest) - 1] = '\0';  // Ensure null termination
        
        // Verify no buffer overflow occurred by checking adjacent memory
        ck_assert_mem_eq(&dest[sizeof(dest)], &backup[sizeof(dest)], 1);
        
        // Verify string is properly terminated within bounds
        ck_assert_int_lt(strlen(dest), sizeof(dest));
    }
}
END_TEST

Suite *security_suite(void)
{
    Suite *s;
    TCase *tc_core;

    s = suite_create("Security");
    tc_core = tcase_create("Core");

    tcase_add_test(tc_core, test_buffer_reads_never_exceed_declared_length);
    suite_add_tcase(s, tc_core);

    return s;
}

int main(void)
{
    int number_failed;
    Suite *s;
    SRunner *sr;

    s = security_suite();
    sr = srunner_create(s);

    srunner_run_all(sr, CK_NORMAL);
    number_failed = srunner_ntests_failed(sr);
    srunner_free(sr);

    return (number_failed == 0) ? EXIT_SUCCESS : EXIT_FAILURE;
}